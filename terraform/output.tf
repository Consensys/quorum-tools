output "quorum-1-public-ip" {
  value = "${aws_instance.quorum_1.public_ip}"
}
output "quorum-1-private-ip" {
  value = "${aws_instance.quorum_1.private_ip}"
}
