FROM erlang

COPY . /game

WORKDIR game

ENTRYPOINT ["rebar3", "shell"]
