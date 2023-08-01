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

### Related Change

Now git-gerrit can do related change.

#### `start-work` can start from a "change-set branch".

"change-set branch" is a branch with name like: 'Change-Id'.c.number-number. git-gerrit submit will automatically create change-set branch.

You can also create a change-set branch from a known numeric change id:

```shell-script
git gerrit unveil-cl change-number patchset-index
git gerrit start-work
```

#### `submit` can be used as usual.

The `submit` will first rename the `'Change-Id'.submit` branch to `'Change-Id'.c.unknown` and then call `git gerrit pull` which will rename the `.unknown` branches to the proper `changenum-patchidx` branches.

#### `whereami`

Show the related-change chain currently in.

#### `pull`

Usage: `git gerrit pull`

Fetch change-sets and the main/master branch from gerrit server.

#### `unveil-cl` 

Usage: `git gerrit unveil-cl changenum patchidx`

Create a change-set branch with name: Change-ID.c.changenum-patchidx and checkout on it. make it ready for start a related-change 

- changenum: numeric change id
- pathcidx: numeric patch index

### How to rebase

#### rebase concepts

1. rebase to head of main/master

```shell-script

A--B--C <- main
   \--D--E--F <- CL

```

After running `git rebase main CL`, it becomes

```shell-script

A--B--C <- main
      \--D`--E`--F` <- CL

```

Note that commits D,E,F in CL branch are all amended.

2. rebase from one CL to another CL

```shell-script
A--B--C <- main
      |--D` <- CL-2
      \--D  <- CL-1
         \--E--F <- CL
```
After running `git rebase --onto CL-2 CL-1 CL`, it becomes
```shell-script
A--B--C  <- main
      |--D` <- CL-2
      |  \--E`--F` <- CL
      \--D  <- CL-1
```

3. rebase from CL to main/master

```shell-script
A--B--C <- main
   \--D  <- CL-1
      \--E--F <- CL
```
After running `git rebase --onto main CL-1 CL`, it becomes

```shell-script
A--B--C <- main
   |   \--E`--F` <- CL
   \--D  <- CL-1

```


#### main/master branch has updates

Rebase your working branch to main/master:

```shell-script
git rebase master
```

Solve the conflicts and do `git rebase --continue`, repeate until all conflicts are solved.

#### related change has a new change-set

Suppose the current work branch name is `Ixxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx`, it is based on changeset branch `Iyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy.c.10001-1`, here the changenum is 10001 and patchnum is 1; suppose the new changeset has a `patchnum` of 2.

Fetch and unveil that new change-set, change num is 10001, patch num is 2:

```shell-script
git gerrit pull
git gerrit unveil-cl 10001 2
```

Rebase to that new change-set:

```shell-script
git rebase --onto Iyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy.c.10001-2 Iyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy.c.10001-1 Ixxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
```

Fix conflicts and do `git rebase --continue`, repeate until all conflicts are fixed.

Now your entire work in `Ixxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx` is based on change-set 10001-2

#### related change has been merged in master

Suppose the current work branch name is `Ixxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx`, it is based on changeset branch `Iyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy.c.10001-1`, here the changenum is 10001 and patchnum is 1; now all works in `Iyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy` has been accepted and in master. 

rebase `Ixxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx` to master:

```shell-script
git rebase --onto master Iyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy.c.10001-1 Ixxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
```
Fix conflicts and do `git rebase --continue`, repeate until all conflicts are fixed.

#### rebase practise

1. Rebase the changeset closer to master first. For example, run `git gerrit whereami`, rebase from the bottom.
1. Squash changes in current branch before rebase, might reduce the efforts in solving conflicts
1. avoid editing the same file in a related-change chain.

### Config values

1. `gerrit.remoteName` the name of a git remote pointing to the gerrit repo, default to `gerrit`
2. `gerrit.remoteRef` the ref of which gerrit review submits to, defaut to `refs/for/main`

## Behind the scenes

The sub-command `start-work` acutally creates a new branch. The development will happen under that branch.

The sub-command `submit` squashes all commits in the current branch and submit it to `gerrit`. The branch name is used as `Change-Id`.

## Use with GitHub

GitHub doesn't have the concept of `CL` nor `change`. Instead, GitHub uses pull-request as a communication point between developers. But the linear git history still can be achieved with GitHub. This tool can be used to get one-commit-per-pull-request as part of practices needed to achieve linear git history. 

### Usage

Setting git config `gerrit.remoteIsNotGerrit` to `true` is to tell this tool that the remote repo is actually not a gerrit.

The mechanism is when running `git gerrit submit`, instead of pushing the `'Change-Id'.submit` branch to `refs/for/main` in the remote repo, this tool will just push the branch `'Change-Id'.submit` to remote repo with the same branch name. 

Then you can create a pull-request from this `'Change-Id'.submit` branch. 


## FAQ

1. `git gerrit submit` failed due to SSO failure(or network failure etc.), what to do?

   - do `git gerrit submit -f` to forcefully resubmit.
