# Branching Strategy

## Main Branch
- The `main` branch is the stable version of the package. All new features and fixes should be merged here only after thorough testing.

## Development Branch (`develop`)
- Use the `develop` branch for ongoing development. New features and non-urgent fixes should be committed here.
- Regularly merge `main` into `develop` to keep it up to date.

```sh
# Switch to develop branch
git checkout develop

# Merge main into develop to keep it updated
git merge main

# Push changes to remote develop branch
git push origin develop
```

## Hotfix Branch

- Create a new branch from `main` for urgent fixes.
- After implementing the fix, create pull requests (PRs) to merge the hotfix branch back into both main and develop.

```sh
# Switch to the main branch
git checkout main

# Pull the latest changes from the remote main branch
git pull origin main

# Create a new hotfix branch from the updated main branch
git checkout -b hotfix

# Stage all changes for commit
git add .

# Commit the changes with a message describing the fix
git commit -m "Fix: description of fix"

# Push the hotfix branch to the remote repository
git push origin hotfix
```
# Merging Process

After pushing the branch, open a Pull Request (PR) on GitHub to tag reviewers and merge into main and issue a new release.

