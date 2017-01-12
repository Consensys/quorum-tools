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
variable "env_name" {
  default = "demo"
}
variable "existing_vpc" { # TODO: replace this with a dyn-generated VPC
  default = "vpc-a48bcac3"
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
variable "quorum_azs" {
  # These are AZes that the existing VPC supports
  default = {
    "1" = "us-east-1b"
    "2" = "us-east-1c"
    "3" = "us-east-1d"
  }
}
variable "existing_subnets" { # TODO: dynamically create these within dyn-generated VPC
  default = {
    "1" = "subnet-6821b445"
    "2" = "subnet-fff25ab6"
    "3" = "subnet-d7b6388c"
  }
}
variable "existing_sg" { # TODO: replace this with a dyn-generated SG
  default = "sg-8a840df7"
}
