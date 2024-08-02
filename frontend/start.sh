#!/bin/bash
DIR=/home/pifort/dev/scheduler/frontend
cd $DIR
export PYENV_ROOT=/home/pifort/.pyenv
echo $PYENV_ROOT
PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
export OPAM_SWITCH_PREFIX=/home/pifort/.opam/default
export PATH="$OPAM_SWITCH_PREFIX/bin:$PATH"
make run
# su pifort && echo $HOME && eval $(opam env --switch=default) && make run


