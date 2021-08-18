simple-twitter
This is a project which has corresponding talk slides

This is a bare bones "Twitter clone" implemented in
a single file that you can deploy to EC2

This uses:

HTML + CSS for the front-end (No JavaScript)
Bootstrap for some minimal styling
A Haskell service for server-side rendering
A Postgres database for application state
NixOps for deployment
AWS EC2 for hosting
The result looks like this:

Screenshot

Instructions
Create an AWS account

... by following these instructions

Install Nix:

$ curl https://nixos.org/nix/install | sh
Install the AWS command-line interface:

$ nix-env --install awscli
Configure your AWS credentials

... by following these instructions

If you did this correctly you should have an ~/.aws/credentials
file that looks similar to this:

[default]
aws_access_key_id = …
aws_secret_access_key = …
Install NixOps:

$ nix-env --install nixops
Build and redeploy the web application

$ nixops create --deployment simple-twitter simple-twitter.nix
$ nixops deploy --deployment simple-twitter --allow-reboot
If you make changes you can redeploy the application by re-running the last
step:

$ nixops deploy --deployment simple-twitter --allow-reboot
To destroy the machine and clean up everything, run:

$ nixops destroy --deployment simple-twitter
$ nixops delete --deployment simple-twitter
Have fun! 🙂

Split files
You can also view the files split out by language:

./split.nix - Only the Nix code
./Main.hs - Only the Haskell code
./initialScript.sql - Only the SQL code
