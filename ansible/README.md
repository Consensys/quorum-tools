To provision some boxes:

- update `inv` file to reflect all hostnames to provision
- provision with `ansible-playbook -i inv site.yml`

This will run `common`, `node`, and `orchestration` provisioning on each node. `node` sets up go & geth. `orchestration` sets up the cluster orchestration code in `quorum-tools`.

During development, if you want to only run orchestration provisioning, run `ansible-playbook -i inv site.yml --tags orch`. If you want to only run node provisioning, run `ansible-playbook -i inv site.yml --tags node`.
