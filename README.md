# The `git gerrit` extension for Git

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

    1. git checkout main
    2. git gerrit start-work
    3. do_some_edits
    4. git commit
    5. do_more_edit3
    6. git commit
    7. git gerrit submit

## Basic Usage

### install

Download this repository and put the `git-gerrit` file in a directory in $PATH and give it executable permission, for example:

```shell-script
install -m 755 git-gerrit /usr/local/bin
```
Run command `git gerrit help` to get a brief introduction of how to run this tool.

### Workflow

The workflow of using `git-gerrit` at a glance:

1.  Create a remote pointing to the gerrit repo, name it `gerrit`

    1.  For existing local git repository
    
        ```shell-script
        git remote add gerrit the-url-to-gerrit-repo
        ```
    1.  For new git clone

        ```shell-script
        git clone --origin gerrit the-url-to-gerrit-repo
        ```

1.  Update the `git` configuration. (change into the git repo directory)

    ```shell-script
    git config --add gerrit.remoteName gerrit
    git config --add gerrit.defaultBranch main
    ```

1.  Adjust the `gerrit.remoteRef` config value to point to the right ref for reviewing

```shell-script
git config --add gerrit.remoteRef refs/for/main
```

1. Start work by using `git gerrit start-work`
1. Do development and commit locally as usual
1. Submit changes as a Change-List(CL) by using `git gerrit submit`
1. Wait for the CL to be reviewed
1. If needed, make more developments and commits and submit the changes by following the previous steps 
1. Submit CL again

### Config values

1. `gerrit.remoteName` the name of a git remote pointing to the gerrit repo, default to `gerrit`
2. `gerrit.remoteRef` the ref of which gerrit review submits to, defaut to `refs/for/main`

## Behind the scenes

The sub-command `start-work` acutally creates a new branch. The development will happen under that branch.

The sub-command `submit` squashes all commits in the current branch and submit it to `gerrit`. The branch name is used as `Change-Id`.
