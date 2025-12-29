:- begin_tests(cmsis_svd).

:- use_module(svd).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

listing(st:device_peripheral_register_field('STM32H747_CM4', _, 'HSEM_CR', _)).

:- dynamic device_peripheral_register_field/4.

device_peripheral_register_field('STM32H747_CM4', 'HSEM', 'HSEM_CR', 'COREID').
device_peripheral_register_field('STM32H747_CM4', 'HSEM', 'HSEM_CR', 'KEY').

listing(st:device_peripheral_register_field_bitOffset('STM32H747_CM4', _, 'HSEM_CR', _, _)).
listing(st:device_peripheral_register_field_bitWidth('STM32H747_CM4', _,
    'HSEM_CR', _, _)).

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic device_peripheral_register_field_bitOffset/5.

device_peripheral_register_field_bitOffset('STM32H747_CM4', 'HSEM', 'HSEM_CR', 'COREID', '8').
device_peripheral_register_field_bitOffset('STM32H747_CM4', 'HSEM', 'HSEM_CR', 'KEY', '16').

:- dynamic device_peripheral_register_field_bitWidth/5.

device_peripheral_register_field_bitWidth('STM32H747_CM4', 'HSEM', 'HSEM_CR', 'COREID', '4').
device_peripheral_register_field_bitWidth('STM32H747_CM4', 'HSEM', 'HSEM_CR', 'KEY', '16').

test(c99, Lines == [ `#define STM32H747_CM4_HSEM_CR_COREID (((1 << 4) - 1) << 8)`,
                     `#define STM32H747_CM4_HSEM_CR_KEY (((1 << 16) - 1) << 16)`
                   ]) :-
    context_module(M),
    findall(Codes, phrase(cmsis_c99_svd(M:_), Codes), Lines).

:- end_tests(cmsis_svd).
