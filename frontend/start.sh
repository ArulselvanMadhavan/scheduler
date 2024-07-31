#!/bin/bash
echo $HOME
curl https://pyenv.run | /usr/bin/bash
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
pyenv install 3.11.2
pip install -r requirements.txt
