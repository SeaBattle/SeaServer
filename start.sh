#!/bin/bash
rebar compile;
rebar generate;
chmod +x rel/seaserver/bin/seaserver;
./rel/seaserver/bin/seaserver console
