provider "aws" {
  access_key = "${var.access_key}"
  secret_key = "${var.secret_key}"
  region = "${var.aws_region}"
}

data "aws_ami" "ubuntu" {
  most_recent = true
  filter {
    name = "name"
    values = ["ubuntu/images/hvm-ssd/ubuntu-xenial-16.04-amd64-server-*"]
  }
  filter {
    name = "virtualization-type"
    values = ["hvm"]
  }
  owners = ["099720109477"] # Canonical
}

resource "aws_vpc" "quorum_raft" {
  cidr_block = "10.0.0.0/16"
  enable_dns_hostnames = true

  tags {
    Project = "${var.project}"
    Name = "${var.project} ${var.env} vpc"
    Environment = "${var.env}"
  }
}

resource "aws_internet_gateway" "quorum_raft" {
  vpc_id = "${aws_vpc.quorum_raft.id}"

  tags {
    Project = "${var.project}"
    Name = "${var.project} ${var.env} internet gateway"
    Environment = "${var.env}"
  }
}

resource "aws_subnet" "a" {
  vpc_id = "${aws_vpc.quorum_raft.id}"

  cidr_block = "10.0.1.0/24"
  availability_zone = "${lookup(var.subnet_azs, "a")}"

  tags {
    Project = "${var.project}"
    Name = "${var.project} ${var.env} subnet A"
    Environment = "${var.env}"
  }
}

resource "aws_subnet" "b" {
  vpc_id = "${aws_vpc.quorum_raft.id}"

  cidr_block = "10.0.2.0/24"
  availability_zone = "${lookup(var.subnet_azs, "b")}"

  tags {
    Project = "${var.project}"
    Name = "${var.project} ${var.env} subnet B"
    Environment = "${var.env}"
  }
}

resource "aws_subnet" "c" {
  vpc_id = "${aws_vpc.quorum_raft.id}"

  cidr_block = "10.0.3.0/24"
  availability_zone = "${lookup(var.subnet_azs, "c")}"

  tags {
    Project = "${var.project}"
    Name = "${var.project} ${var.env} subnet C"
    Environment = "${var.env}"
  }
}

resource "aws_route_table" "quorum_raft" {
  vpc_id = "${aws_vpc.quorum_raft.id}"

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = "${aws_internet_gateway.quorum_raft.id}"
  }

  tags {
    Project = "${var.project}"
    Name = "${var.project} ${var.env} route table"
    Environment = "${var.env}"
  }
}

resource "aws_route_table_association" "a" {
  subnet_id = "${aws_subnet.a.id}"
  route_table_id = "${aws_route_table.quorum_raft.id}"
}

resource "aws_route_table_association" "b" {
  subnet_id = "${aws_subnet.b.id}"
  route_table_id = "${aws_route_table.quorum_raft.id}"
}

resource "aws_route_table_association" "c" {
  subnet_id = "${aws_subnet.c.id}"
  route_table_id = "${aws_route_table.quorum_raft.id}"
}

resource "aws_security_group" "ssh_open" {
  name = "${var.project} ${var.env} ssh access"
  description = "Allow ssh connections"
  vpc_id = "${aws_vpc.quorum_raft.id}"

  ingress {
    from_port = 22
    to_port = 22
    protocol = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags {
    Project = "${var.project}"
    Name = "${var.project} ${var.env} ssh access"
  }
}

resource "aws_security_group" "rpc_sender" {
  name = "${var.project} ${var.env} rpc sender"
  description = "Can send RPC traffic to ${var.project} quorum nodes"
  vpc_id = "${aws_vpc.quorum_raft.id}"

  tags {
    Project = "${var.project}"
    Name = "${var.project} ${var.env} rpc sender"
  }
}

resource "aws_security_group" "quorum_instance" {
  name = "${var.project} ${var.env} quorum instance"
  description = "Allow eth p2p from other quorum nodes and RPC traffic from designated nodes"
  vpc_id = "${aws_vpc.quorum_raft.id}"

  ingress {
    from_port = 30400
    to_port = 30900
    protocol = "tcp"
    self = true # incoming traffic comes from this same security group
  }

  ingress {
    from_port = 40400
    to_port = 40900
    protocol = "tcp"
    security_groups = ["${aws_security_group.rpc_sender.id}"]
  }

  ingress {
    from_port = 50400
    to_port = 50900
    protocol = "tcp"
    self = true # incoming traffic comes from this same security group
  }

  egress {
    from_port = 0
    to_port = 0
    protocol = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags {
    Project = "${var.project}"
    Name = "${var.project} ${var.env} quorum instance"
  }
}

resource "aws_iam_role" "ecr_accessor" {
  name = "${var.project}.${var.env}.ecrAccessor"
  assume_role_policy = <<EOF
{ "Version":   "2012-10-17"
, "Statement": [ { "Effect":    "Allow"
                 , "Principal": { "Service": "ec2.amazonaws.com" }
                 , "Action":    "sts:AssumeRole" } ] }
EOF
}

resource "aws_iam_role_policy" "ecr_accessor" {
  name = "${var.project}.${var.env}.ecrAccessor"
  role = "${aws_iam_role.ecr_accessor.id}"
  policy = <<EOF
{ "Version":   "2012-10-17"
, "Statement": [ { "Action":   [ "ecr:GetAuthorizationToken"
                               , "ecr:BatchCheckLayerAvailability"
                               , "ecr:GetDownloadUrlForLayer"
                               , "ecr:BatchGetImage"
                               ]
                 , "Effect":   "Allow"
                 , "Resource": "*" } ] }
EOF
}

resource  "aws_iam_instance_profile" "ecr_accessor" {
  name = "${var.project}.${var.env}.ecrAccessor"
  roles = ["${aws_iam_role.ecr_accessor.name}"]
}

resource "aws_instance" "quorum_1" {
  ami = "${data.aws_ami.ubuntu.id}"
  availability_zone = "${aws_subnet.a.availability_zone}"
  instance_type = "${lookup(var.instance_types, "quorum")}"
  iam_instance_profile = "${aws_iam_instance_profile.ecr_accessor.id}"
  #
  # NOTE: rpc_sender is in this list temporarily, until we provision nodes that are dedicated to send txes
  #
  vpc_security_group_ids = ["${aws_security_group.quorum_instance.id}", "${aws_security_group.ssh_open.id}", "${aws_security_group.rpc_sender.id}"]
  key_name = "${var.ssh_keypair_name}"
  subnet_id = "${aws_subnet.a.id}"
  associate_public_ip_address = true

  root_block_device {
    volume_type = "${lookup(var.volume_types, "quorum")}"
    volume_size = "${lookup(var.volume_sizes, "quorum")}"
    delete_on_termination = "true"
  }

  tags {
    Project = "${var.project}"
    Name = "${var.project} ${var.env} quorum 1"
    Environment = "${var.env}"
  }
}

resource "aws_instance" "quorum_2" {
  ami = "${data.aws_ami.ubuntu.id}"
  availability_zone = "${aws_subnet.b.availability_zone}"
  instance_type = "${lookup(var.instance_types, "quorum")}"
  iam_instance_profile = "${aws_iam_instance_profile.ecr_accessor.id}"
  # NOTE: rpc_sender is currently not in this list:
  vpc_security_group_ids = ["${aws_security_group.quorum_instance.id}", "${aws_security_group.ssh_open.id}"]
  key_name = "${var.ssh_keypair_name}"
  subnet_id = "${aws_subnet.b.id}"
  associate_public_ip_address = true

  root_block_device {
    volume_type = "${lookup(var.volume_types, "quorum")}"
    volume_size = "${lookup(var.volume_sizes, "quorum")}"
    delete_on_termination = "true"
  }

  tags {
    Project = "${var.project}"
    Name = "${var.project} ${var.env} quorum 2"
    Environment = "${var.env}"
  }
}

resource "aws_instance" "quorum_3" {
  ami = "${data.aws_ami.ubuntu.id}"
  availability_zone = "${aws_subnet.c.availability_zone}"
  instance_type = "${lookup(var.instance_types, "quorum")}"
  iam_instance_profile = "${aws_iam_instance_profile.ecr_accessor.id}"
  # NOTE: rpc_sender is currently not in this list:
  vpc_security_group_ids = ["${aws_security_group.quorum_instance.id}", "${aws_security_group.ssh_open.id}"]
  key_name = "${var.ssh_keypair_name}"
  subnet_id = "${aws_subnet.c.id}"
  associate_public_ip_address = true

  root_block_device {
    volume_type = "${lookup(var.volume_types, "quorum")}"
    volume_size = "${lookup(var.volume_sizes, "quorum")}"
    delete_on_termination = "true"
  }

  tags {
    Project = "${var.project}"
    Name = "${var.project} ${var.env} quorum 3"
    Environment = "${var.env}"
  }
}
