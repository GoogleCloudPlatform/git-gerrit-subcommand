# The `git gerrit` extension for Git

The reworked version of git-gerrit.


This plugin makes the workflow with gerrit easier by automating "branch then squash-merge".

## Why this tool

The usual workflow when using gerrit suggests submit one single commit at a time for code review.

Often users are suggested to use `git commit --amend -a` each time they do edits, the workflow would be like:

    1. git checkout main
    2. do_some_edits
    3. git commit
    4. do_more_edits
    5. git commit --amend
    6. git push gerrit main:refs/for/main

In this way, the local commit history is lost. In compare to the pull-request workflow:

    1. git checkout -b feature -u main
    2. do_some_edits
    3. git commit
    4. do_more_edits
    5. git commit
    6. git request-pull(or create pull-request/merge-request in github/gitlab)

This workflow allows you to preserve local history, so the developer may consult the history later.

The purpose of creating this tool is to provide an extension to the `git` command and change the workflow of using gerrit to be like this:

    1. git init gerrit-url .
    2. git gerrit workspace-start
    3. do_some_edits
    4. git commit
    5. do_more_edit3
    6. git commit
    7. git gerrit upload

## Features

- [DONE] Workspace create/delete/sync
- [DONE] Upload Change
- [DONE] Rebase workspace to HEAD or newest patchset of related-change
- [TODO] Sparse checkout when creating workspace
- [TODO] Integrate with VSCode
- [TODO] A fuse filesystem to do lazy checkouts to save time

## Basic Usage

### install

Download the binaries from this repository and put the `git-gerrit` file in a directory in $PATH and give it executable permission, for example:

```shell-script
install -m 755 git-gerrit /usr/local/bin
```
Run command `git gerrit -h` to get a brief introduction of how to run this tool.

### Concepts

1. workplace

A directory holding the bare git repo and the workspaces. Can be initialized by running:

```shell-script
git gerrit init repo-url
```

2. change

A change in gerrit is a set of modifies to be reviewed. This this tool, a change is bound to a git worktree and a branch. It can be started by `git gerrit start-change`, deleted by `git gerrit delete-change`. The change can also be uploaded: `git gerrit upload` and the change message can be modified `git gerrit modify-desc`.

### Workflow

The workflow of using `git gerrit` at a glance:

1. Create an empty directory for all the works.
2. Init the workplace

   ```
   git gerrit init <git-repo-url>
   ```


1. Start work by using `git gerrit workspace-start <workspace-name>`
1. Enter the newly created directory `<workspace-name>`
1. Do development and commit locally as usual
1. Upload change by using `git gerrit upload`
1. Wait for the change to be reviewed
1. If needed, make more developments and commits and submit the changes by following the previous steps 
1. Upload change again

### Related Change

Now git-gerrit can do related change.

#### `workspace-start` can start from a "related change".

Find the number of the change you want relate your new work from, then 

```shell-script
git gerrit workspace-start -r <relate-change> <workspace-name>
```

### Rebase

The requirement of rebasing happens when the main branch has updated or related change has a new patchset uploaded. 

It is as simple as issuing `git gerrit rebase` in your workspace to rebase your change.

The behavior of this sub-command is as following in three cases:

1. The current change was started from main (no related change) and main has updates.
   This case the rebase is to the HEAD of main.

2. The current change was related to another change, that change has a new patchset. 
   This case the rebase is done like `git rebase --onto new-patchset old-patchset`. 
   
3. The current change was related to another change, but that change has already been uploaded. 
   
   This case the rebase is to the HEAD of main.

## Commands explanation

### `version`

Print the version of this tool.

### `init`

Init the workplace.

### `sync`

Synchronize the repository with the remote.

### `workspace-start`

Start a new change by creating a workspace(combined by a git worktree and a branch).

### `rebase`

Rebase the change.

### `status`

Print the status of current workspace.

### `upload`

Upload the change.

### `modify-desc` 

Modify the commit message of the change

### `workspace-delete`

Delete the workspace. 



