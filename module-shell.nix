# Edit the module name here, then run:
#
# nix-shell module-shell.nix
#
# Alternatively, specify the module on the command line:
#
# nix-shell dependencies.nix -A ortholang-seqio

(import ./dependencies.nix).ortholang-seqio
