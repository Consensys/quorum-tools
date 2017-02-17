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

# Output for single-region IPs
output "geth1" { value = "${ var.multi_region ? "" : aws_instance.quorum.0.public_ip }" }
output "geth2" { value = "${ var.multi_region ? "" : aws_instance.quorum.1.public_ip }" }
output "geth3" { value = "${ var.multi_region ? "" : aws_instance.quorum.2.public_ip }" }

# Output for multi-region IP
output "eip" {
  value = "${element(aws_eip_association.quorum_eip_association.*.public_ip, 0)}"
}
