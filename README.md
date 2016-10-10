# tracetool
Trace tool for Erlang



#Loadtest:

cp src/plot.pg ebin/
erl -pa ./ebin

1 > code:load_file(loadtest).
2 > tracetool:start("src/loadtest.conf").
3 > loadtest:start_link().
