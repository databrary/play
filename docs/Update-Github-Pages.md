The [GitHub Page for the Databrary
project](http://databrary.github.io/databrary/) holds developer
documentation and some internal reports. This page documents how to
update it.

## The automatic update process

Run the script "./s/update-developer-docs" from the Databrary repo.

## Details

The script above does three things to update the gh-pages branch.

1.  It builds the app with coverage reporting and haddocks enabled.
2.  It copies the report into the coverage/ subdirectory and the
    haddocks into the haddocks/ subdirectory of gh-pages.
3.  It pulls in the latest JavaScript documents into gh-pages with a
    "git submodule update --remote".

The script also pushes the gh-pages branch to GitHub, deploying the new
docs.

### Example: Only updating frontend docs

Rather than using the automated script, you can do one of the above
steps individually. So, to update frontend docs, just do the third step
by itself.

Well, that's not the whole story. There are some preliminary steps as
well. The full set of steps to update the frontend docs alone would be:

1.  Clone or update the databrary repository.
2.  Make your worktree totally clean, either with "git stash save
    --all", or by using "git worktree . . .".
3.  Check out the gh-pages branch.
4.  Pull in the latest frontend docs with "git submodule update
    --remote".
5.  Stage all changes (additions and deletions) with "git add --all".
6.  Commit the changes and push the branch to GitHub.
