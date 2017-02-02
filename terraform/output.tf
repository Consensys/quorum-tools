output "environment" {
  value = "${var.env}"
}
output "quorum-public-ips" {
  value = ["${aws_instance.quorum.*.public_ip}"]
}
output "quorum-private-ips" {
  value = ["${aws_instance.quorum.*.private_ip}"]
}
output "quorum-azs" {
  value = ["${aws_instance.quorum.*.availability_zone}"]
}

output "geth1" { value = "${aws_instance.quorum.0.public_ip}" }
output "geth2" { value = "${aws_instance.quorum.1.public_ip}" }
output "geth3" { value = "${aws_instance.quorum.2.public_ip}" }
