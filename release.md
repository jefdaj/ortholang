Main OrthoLang release checklist:

- [ ] checkout release branch from develop
- [ ] bump version in OrthoLang.cabal + ortholang.nix
- [ ] update nixpkgs if needed
- [ ] tag, push, and pin new nixpkgs if needed
- [ ] build + run/update tests on linux (use stack, then nix)
- [ ] build + run/update tests on mac (use stack, then nix)
- [ ] cachix push build + shell packages on linux
- [ ] cachix push build + shell packages on mac
- [ ] push release branch to Travis and check that version tests pass
- [ ] merge --squash release branch into master
- [ ] push master to github (+ travis)
- [ ] dev-scripts/build-release-archives.sh, then upload to github release page
- [ ] dev-scripts/build-docker-image.sh (this one is slow!)
- [ ] if docker tests pass, push image to docker-hub
- [ ] update changelog
- [ ] paste new changelog notes into github release notes
- [ ] edit install.sh to point to uploaded archive URL and push --force
- [ ] merge release branch back into develop
- [ ] rebase all the module branches from develop and update their tests
- [ ] ideally, check that install.sh works from a new computer

Finally, update the demo site:
- [ ] update ortholang submodule commit to point to master
- [ ] bump expected version in ortholang-demo.py
- [ ] build and run the docs executable to update functions\*.md
- [ ] restart the demo and check that it works
