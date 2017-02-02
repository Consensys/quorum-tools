variable "env" {
  description = "Name of the environment"
  # this value is set by bin/.bin/env-wrapper
}
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
variable "instance_types" {
  default = {
    quorum = "m4.large"
    # TODO: other types of nodes, like for metrics or rpc (tx) senders
  }
}
variable "volume_types" {
  default = {
    quorum = "gp2"
  }
}
variable "volume_sizes" {
  default = {
    quorum = "50"
  }
}
variable "subnet_azs" {
  type = "list"
  default = ["us-east-1b", "us-east-1c", "us-east-1d"]
}
variable "num_instances" {
  default = 3
}
variable "local_datadir_root" {
  default = "cluster-data"
}
variable "remote_user" {
  default = "ubuntu"
}
variable "remote_homedir" {
  default = "/home/ubuntu"
}
variable "pem_file" {
  default = "../credentials/ethereum-raft-demo.pem"
}
