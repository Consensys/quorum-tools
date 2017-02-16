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
    Terraformed = "true"
  }
}

resource "aws_internet_gateway" "quorum_raft" {
  vpc_id = "${aws_vpc.quorum_raft.id}"

  tags {
    Project = "${var.project}"
    Name = "${var.project} ${var.env} internet gateway"
    Environment = "${var.env}"
    Terraformed = "true"
  }
}

resource "aws_subnet" "subnet" {
  count = "${length(var.subnet_azs)}"

  vpc_id = "${aws_vpc.quorum_raft.id}"

  cidr_block = "10.0.${count.index + 1}.0/24"
  availability_zone = "${element(var.subnet_azs, count.index)}"

  tags {
    Project = "${var.project}"
    Name = "${var.project} ${var.env} subnet ${count.index + 1}"
    Environment = "${var.env}"
    Terraformed = "true"
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
    Terraformed = "true"
  }
}

resource "aws_route_table_association" "rta" {
  count = "${length(var.subnet_azs)}"

  subnet_id = "${element(aws_subnet.subnet.*.id, count.index)}"
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
    Environment = "${var.env}"
    Terraformed = "true"
  }
}

resource "aws_security_group" "rpc_sender" {
  name = "${var.project} ${var.env} rpc sender"
  description = "Can send RPC traffic to ${var.project} quorum nodes"
  vpc_id = "${aws_vpc.quorum_raft.id}"

  tags {
    Project = "${var.project}"
    Name = "${var.project} ${var.env} rpc sender"
    Environment = "${var.env}"
    Terraformed = "true"
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
    Environment = "${var.env}"
    Terraformed = "true"
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

resource "null_resource" "cluster_datadirs" {
  triggers {
    num_instances = "${var.num_instances}"
    subnet_azs = "${join(",", var.subnet_azs)}"
    local_datadir_root = "${var.local_datadir_root}"
  }

  provisioner "local-exec" {
    command = "stack exec -- aws-bootstrap --nodes ${var.num_instances} --subnets ${length(var.subnet_azs)} --path ${var.local_datadir_root}"
  }
}

resource "aws_instance" "quorum" {
  count = "${var.num_instances}"
  depends_on = ["null_resource.cluster_datadirs"]

  ami = "${data.aws_ami.ubuntu.id}"
  instance_type = "${lookup(var.instance_types, "quorum")}"
  iam_instance_profile = "${aws_iam_instance_profile.ecr_accessor.id}"
  #
  # NOTE: rpc_sender is in this list temporarily, until we provision nodes that are dedicated to send txes
  #
  vpc_security_group_ids = ["${aws_security_group.quorum_instance.id}", "${aws_security_group.ssh_open.id}", "${aws_security_group.rpc_sender.id}"]
  key_name = "${var.ssh_keypair_name}"
  associate_public_ip_address = true

  availability_zone = "${element(aws_subnet.subnet.*.availability_zone, count.index % length(aws_subnet.subnet.*.id))}"
  subnet_id =         "${element(aws_subnet.subnet.*.id,                count.index % length(aws_subnet.subnet.*.id))}"

  private_ip = "${cidrhost(element(aws_subnet.subnet.*.cidr_block, count.index % length(aws_subnet.subnet.*.id)), 101 + (count.index / length(aws_subnet.subnet.*.id)))}"

  root_block_device {
    volume_type = "${lookup(var.volume_types, "quorum")}"
    volume_size = "${lookup(var.volume_sizes, "quorum")}"
    delete_on_termination = "true"
  }

  tags {
    Project = "${var.project}"
    Name = "${var.project} ${var.env} quorum ${count.index + 1}"
    Environment = "${var.env}"
    Terraformed = "true"
  }

  connection {
    user = "${var.remote_user}"
    host = "${self.public_ip}"
    timeout = "1m"
    key_file = "${var.pem_file}"
  }

  provisioner "file" {
    source = "${var.local_datadir_root}/geth${var.first_geth_id + count.index}"
    destination = "${var.remote_homedir}/datadir"
  }

  provisioner "file" {
    source = "../credentials/id_tunneler"
    destination = "${var.remote_homedir}/.ssh/id_tunneler"
  }

  provisioner "file" {
    source = "../credentials/id_tunneler.pub"
    destination = "${var.remote_homedir}/.ssh/id_tunneler.pub"
  }

  provisioner "remote-exec" {
    inline = [
      "echo '${var.first_geth_id + count.index}' >node-id",
      "echo 'abcd' >password",
      "echo '${var.multi_region ? "multi-region" : "single-region"}' >cluster-type"
    ]
  }

  provisioner "remote-exec" {
    scripts = [
      "scripts/prepare.sh",
      "scripts/fetch-quorum-image.sh",
      "scripts/fetch-harness-image.sh",
      "scripts/start-quorum.sh"
    ]
  }
}

resource "aws_eip_association" "quorum_eip_association" {
  count = "${length(var.quorum_eip_ids)}"
  instance_id = "${element(aws_instance.quorum.*.id, count.index)}"
  allocation_id = "${element(var.quorum_eip_ids, count.index)}"
}
