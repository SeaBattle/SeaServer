## Websocket api interface

### Packaging
All packages are erlang maps with binary keys and any values. They are 
decoded to msgpack before sending to user. Every package should have 
`packet_type` header - where package type is specified. Also `packet_id` 
field is forwarded in response, if exists in request.

#### Authentification
Users are authenticated by tokens, provided by `UserService`. Only this 
package is allowed to unauthenticated user. To authenticate - auth package
is sent:

    {"packet_type" : 100, "uid" : SomeUID, "token" : SomeToken}
Where `SomeUID` and `SomeToken` you got from `UserService`. Probably they
are binaries.
    
#### Creating a game

#### Inviting to game

#### Accepting|rejecting an invite

#### Joining new|existing game

#### Send ships to game

#### Starting a game

#### Make a fire

#### Fast play a game

#### Getting an error
There is a special error package:

    {"packet_type" : 400, "code" : SomeCode}
Where `SomeCode` is an integer code of error.