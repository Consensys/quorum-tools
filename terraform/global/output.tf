output "iam-instance-profile-id" {
  value = "${aws_iam_instance_profile.ecr_accessor.id}"
}
