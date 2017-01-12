output "quorum-1-public-ip" {
  value = "${aws_instance.quorum_1.public_ip}"
}
output "quorum-1-private-ip" {
  value = "${aws_instance.quorum_1.private_ip}"
}
output "quorum-1-az" {
  value = "${aws_instance.quorum_1.availability_zone}"
}
output "quorum-2-public-ip" {
  value = "${aws_instance.quorum_2.public_ip}"
}
output "quorum-2-private-ip" {
  value = "${aws_instance.quorum_2.private_ip}"
}
output "quorum-2-az" {
  value = "${aws_instance.quorum_2.availability_zone}"
}
output "quorum-3-public-ip" {
  value = "${aws_instance.quorum_3.public_ip}"
}
output "quorum-3-private-ip" {
  value = "${aws_instance.quorum_3.private_ip}"
}
output "quorum-3-az" {
  value = "${aws_instance.quorum_3.availability_zone}"
}
