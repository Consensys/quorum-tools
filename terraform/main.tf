provider "aws" {
  access_key = "${var.access_key}"
  secret_key = "${var.secret_key}"
  region = "${var.aws_region}"
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

# TODO: dynamically create security group for quorum nodes
# TODO: dynamically create security group for open SSH

# resource "aws_instance" "quorum_1" {
#   ami = "${var.ec2_ami}"
#   availability_zone = "${aws_subnet.a.availability_zone}"
#   instance_type = "${lookup(var.instance_types, "quorum")}"
#   security_groups = ["${var.existing_sg}"]
#   key_name = "${var.ssh_keypair_name}"
#   subnet_id = "${aws_subnet.a.id}"
#   associate_public_ip_address = true
#
#   tags {
#     Project = "${var.project}"
#     Name = "${var.project} ${var.env} quorum 1"
#     Environment = "${var.env}"
#   }
# }
