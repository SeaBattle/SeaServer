#!/bin/bash
rebar compile;
rebar generate;
./rel/seaserver/bin/seaserver console
