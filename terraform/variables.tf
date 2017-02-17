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
variable "precreated_global_quorum_iam_instance_profile_id" {
  default = "quorum-raft.global.ecrAccessor" # This was the output of provisioning the permanent "global" IAM infrastructure.
}

#
# Variables that can be overridden by multi-region settings (see: multi-region-vars/*.tfvars):
#
variable "multi_region" {
  description = "Whether the cluster spans AWS regions. This is a boolean represented as a string until TF supports first-class booleans."
  default = "0"
}
variable "aws_region" {
  description = "AWS region"
  default = "us-east-1"
}
variable "subnet_azs" {
  type = "list"
  default = ["us-east-1b", "us-east-1c", "us-east-1d"]
}
variable "num_instances" {
  default = 3
}
variable "quorum_eip_ids" {
  description = "Pre-allocated elastic IP( ID)s to be associated with quorum nodes. This is primarily for supporting multi-region clusters."
  default = []
}
variable "first_geth_id" {
  description = "Amount to add to the instance's count.index to calculate gethId. This is primarily for supporting multi-region clusters."
  default = 1
}
# [End of variables overridden by multi-region settings.]
