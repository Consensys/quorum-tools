variable "access_key" {
  description = "AWS access key"
  # this value comes from secrets.tfvars
}
variable "secret_key" {
  description = "AWS secret key"
  # this value comes from secrets.tfvars
}
variable "aws_region" {
  description = "AWS region"
  default = "us-east-1"
}
