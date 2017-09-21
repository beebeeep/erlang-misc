-module(elf).
-export([dump/1]).


dump(File) when is_list(File) ->
    case file:read_file(File) of
        {ok, Data} -> dump(Data);
        {error, Why} -> {error, Why}
    end;

dump(<<16#7f, "ELF", Data/binary>>) ->
    case Data of
        % 32-bit little-endian
        <<1:8, 1:8, Ei_version:8, Ei_osabi:8, Ei_abiversion:8,
          _Ei_pad:56, E_type:16, E_machine:16, E_version:32,
          E_entry:32/little, E_phoff:32/little, E_shoff:32/little,
          E_flags:32/little, E_ehsize:16/little, E_phentsize:16/little, E_phnum:16/little,
          E_shentsize:16/little, E_shnum:16/little, E_shstrndx:16/little, _Rest/binary>> ->
            show_elf_info(32, little, Ei_version, Ei_osabi, Ei_abiversion,
                          E_type, E_machine, E_version, E_entry, E_phoff, E_shoff,
                          E_flags, E_ehsize, E_phentsize, E_phnum, E_shentsize,
                          E_shnum, E_shstrndx);

        % 32-bit big-endian
        <<1:8, 2:8, Ei_version:8, Ei_osabi:8, Ei_abiversion:8,
          _Ei_pad:56, E_type:16, E_machine:16, E_version:32,
          E_entry:32/big, E_phoff:32/big, E_shoff:32/big,
          E_flags:32/big, E_ehsize:16/big, E_phentsize:16/big, E_phnum:16/big,
          E_shentsize:16/big, E_shnum:16/big, E_shstrndx:16/big, _Rest/binary>> ->
            show_elf_info(32, big, Ei_version, Ei_osabi, Ei_abiversion,
                          E_type, E_machine, E_version, E_entry, E_phoff, E_shoff,
                          E_flags, E_ehsize, E_phentsize, E_phnum, E_shentsize,
                          E_shnum, E_shstrndx);

        % 64-bit big-endian
        <<2:8, 2:8, Ei_version:8, Ei_osabi:8, Ei_abiversion:8,
          _Ei_pad:56, E_type:16, E_machine:16, E_version:32,
          E_entry:64/big, E_phoff:64/big, E_shoff:64/big,
          E_flags:32/big, E_ehsize:16/big, E_phentsize:16/big, E_phnum:16/big,
          E_shentsize:16/big, E_shnum:16/big, E_shstrndx:16/big, _Rest/binary>> ->
            show_elf_info(64, big, Ei_version, Ei_osabi, Ei_abiversion,
                          E_type, E_machine, E_version, E_entry, E_phoff, E_shoff,
                          E_flags, E_ehsize, E_phentsize, E_phnum, E_shentsize,
                          E_shnum, E_shstrndx);

        % 64-bit little-endian
        <<2:8, 1:8, Ei_version:8, Ei_osabi:8, Ei_abiversion:8,
          _Ei_pad:56, E_type:16, E_machine:16, E_version:32,
          E_entry:64/little, E_phoff:64/little, E_shoff:64/little,
          E_flags:32/little, E_ehsize:16/little, E_phentsize:16/little, E_phnum:16/little,
          E_shentsize:16/little, E_shnum:16/little, E_shstrndx:16/little, _Rest/binary>> ->
            show_elf_info(32, little, Ei_version, Ei_osabi, Ei_abiversion,
                          E_type, E_machine, E_version, E_entry, E_phoff, E_shoff,
                          E_flags, E_ehsize, E_phentsize, E_phnum, E_shentsize,
                          E_shnum, E_shstrndx)
    end;

dump(Data) when is_binary(Data) ->
    not_elf.

show_elf_info(WordLength, Endianess, Ei_version, Ei_osabi, Ei_abiversion,
              E_type, E_machine, E_version, E_entry, E_phoff, E_shoff,
              E_flags, E_ehsize, E_phentsize, E_phnum, E_shentsize,
              E_shnum, E_shstrndx) -> 
    ok.
