provider "aws" {
  access_key = "${var.access_key}"
  secret_key = "${var.secret_key}"
  region = "${var.aws_region}"
}

resource "aws_iam_role" "ecr_accessor" {
  name = "${var.project}.${var.env}.ecrAccessor"
  assume_role_policy = <<EOF
{ "Version":   "2012-10-17"
, "Statement": [ { "Effect":    "Allow"
                 , "Principal": { "Service": "ec2.amazonaws.com" }
                 , "Action":    "sts:AssumeRole" } ] }
EOF
}

resource "aws_iam_role_policy" "ecr_accessor" {
  name = "${var.project}.${var.env}.ecrAccessor"
  role = "${aws_iam_role.ecr_accessor.id}"
  policy = <<EOF
{ "Version":   "2012-10-17"
, "Statement": [ { "Action":   [ "ecr:GetAuthorizationToken"
                               , "ecr:BatchCheckLayerAvailability"
                               , "ecr:GetDownloadUrlForLayer"
                               , "ecr:BatchGetImage"
                               ]
                 , "Effect":   "Allow"
                 , "Resource": "*" } ] }
EOF
}

resource  "aws_iam_instance_profile" "ecr_accessor" {
  name = "${var.project}.${var.env}.ecrAccessor"
  roles = ["${aws_iam_role.ecr_accessor.name}"]
}
