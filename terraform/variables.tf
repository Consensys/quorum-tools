variable "access_key" {
  description = "AWS access key"
  # this value comes from terraform.tfvars
}
variable "secret_key" {
  description = "AWS secret key"
  # this value comes from terraform.tfvars
}
variable "ssh_keypair_name" {
  description = "Name of the SSH keypair for logging into instances"
  default = "ethereum-raft-demo"
}
variable "aws_region" {
  description = "AWS region"
  default = "us-east-1"
}
variable "project" {
  default = "quorum-raft"
}
variable "env" {
  default = "demo"
}
variable "ec2_ami" {
  default = "ami-40d28157" # ubuntu/images/hvm-ssd/ubuntu-xenial-16.04-amd64-server-20161020
}
variable "instance_types" {
  default = {
    quorum = "t2.small"
    # TODO: other types of nodes, like for metrics or clients sending txes
  }
}
variable "subnet_azs" {
  default = {
    "a" = "us-east-1b"
    "b" = "us-east-1c"
    "c" = "us-east-1d"
  }
}
#
# TODO: replace this with a dyn-generated SG
#
variable "existing_sg" {
  default = "sg-8a840df7"
}
