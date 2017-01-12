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
variable "instance_types" {
  default = {
    quorum = "t2.small"
    # TODO: other types of nodes, like for metrics or rpc (tx) senders
  }
}
variable "subnet_azs" {
  default = {
    "a" = "us-east-1b"
    "b" = "us-east-1c"
    "c" = "us-east-1d"
  }
}
