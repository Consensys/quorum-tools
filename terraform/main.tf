provider "aws" {
  access_key = "${var.access_key}"
  secret_key = "${var.secret_key}"
  region = "${var.aws_region}"
}

# TODO: dynamically create VPC
# TODO: dynamically create subnets in VPC
# TODO: dynamically create security group for quorum nodes
# TODO: dynamically create security group for open SSH

resource "aws_instance" "quorum_1" {
  ami = "${var.ec2_ami}"
  availability_zone = "${lookup(var.quorum_azs, "1")}"
  instance_type = "${lookup(var.instance_types, "quorum")}"
  security_groups = ["${var.existing_sg}"]
  key_name = "${var.ssh_keypair_name}"
  subnet_id = "${lookup(var.existing_subnets, "1")}"
  associate_public_ip_address = true

  tags {
    Project = "quorum-raft"
    Name = "quorum-raft 1"
    Environment = "${var.env_name}"
  }
}
