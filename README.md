eapi - DEPRECATED
=====

eapi is now deprecated, for the C side use the dthread library instead, 
for the Erlang side manual labour is required.

### Dependencies

To build eapi you will need a working installation of Erlang R15B (or
later).<br/>
Information on building and installing [Erlang/OTP](http://www.erlang.org)
can be found [here](https://github.com/erlang/otp/wiki/Installation)
([more info](https://github.com/erlang/otp/blob/master/INSTALL.md)).

eapi is built using rebar that can be found [here](https://github.com/basho/rebar), with building instructions [here](https://github.com/basho/rebar/wiki/Building-rebar).

eapi also requires the following applications to be installed:
<ul>
<li>dthread - https://github.com/tonyrog/dthread</li>
<li>uart - https://github.com/tonyrog/uart</li>
</ul>


### Downloading

Clone the repository in a suitable location:

```sh
$ git clone git://github.com/tonyrog/eapi.git
```
### Configurating
#### Concepts

...

#### Files

...

### Building

Compile:

```sh
$ cd eapi
$ rebar compile
...
==> eapi (compile)
```


