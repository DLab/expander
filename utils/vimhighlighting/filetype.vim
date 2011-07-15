" my filetype file
  if exists("did_load_filetypes")
    finish
  endif
  augroup filetype
    au! BufRead,BufNewFile *.kappa,*.ka setfiletype kappa
    au! BufRead,BufNewFile *.pka setfiletype prekappa
  augroup END
