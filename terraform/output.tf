output "multi-region-cluster" {
  value = "${var.multi_region}"
}
output "aws-region" {
  value = "${var.aws_region}"
}
output "environment" {
  value = "${var.env}"
}
output "quorum-private-ips" {
  value = ["${aws_instance.quorum.*.private_ip}"]
}
output "quorum-azs" {
  value = ["${aws_instance.quorum.*.availability_zone}"]
}

# Yes, this is insane. It's not easy with this language. This yields an EIP if it was allocated, otherwise standard public IP if it was allocated, or an empty string. For some reason accessing the EIP list out of bounds always gives you the last element if one exists. Also I can't figure out how to prevent some (e.g. "empty") outputs from showing up.
#
# Technically we don't need code this complicated if we ask users to manually refresh, because "public_ip" will eventually/asynchronously become an EIP if allocated, but that'd be a pretty bad UX
output "geth1" {
  value = "${length(var.quorum_eip_ids) > (0 + var.first_geth_id - 1) ? element(aws_eip_association.quorum_eip_association.*.public_ip, (0 + var.first_geth_id - 1)) : (var.num_instances > 0 ? element(aws_instance.quorum.*.public_ip, (0 + var.first_geth_id - 1)) : "")}"
}
output "geth2" {
  value = "${length(var.quorum_eip_ids) > (1 + var.first_geth_id - 1) ? element(aws_eip_association.quorum_eip_association.*.public_ip, (1 + var.first_geth_id - 1)) : (var.num_instances > 1 ? element(aws_instance.quorum.*.public_ip, (1 + var.first_geth_id - 1)) : "")}"
}
output "geth3" {
  value = "${length(var.quorum_eip_ids) > (2 + var.first_geth_id - 1) ? element(aws_eip_association.quorum_eip_association.*.public_ip, (2 + var.first_geth_id - 1)) : (var.num_instances > 2 ? element(aws_instance.quorum.*.public_ip, (2 + var.first_geth_id - 1)) : "")}"
}
