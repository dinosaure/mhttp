# A little HTTP stack from [`httpcats`][httpcats] and compatible with unikernels

`mhttp` is a small derivative of the [`httpcats`][httpcats] library
(implementing an HTTP client and server) with the [`mnet`][mnet] TCP/IP stack
(which uses [`utcp`][utcp] and [`mkernel`][mkernel]).

[httpcats]: https://github.com/robur-coop/httpcats
[mnet]: https://git.robur.coop/robur/mnet
[mkernel]: https://git.robur.coop/robur/mkernel
[utcp]: https://github.com/robur-coop/utcp
