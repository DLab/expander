:runtime! syntax/kappa.vim

:syntax match prekappaLocfield /%\([^(]*)\)\@=/
:syntax match prekappaKeyword /%\(loc\|org\|dst\)/ contained
:syntax match prekappaLabel /%\(loc\|org\|dst\)\(\[\)\@=/
:syntax match prekappaLabel /%cell/
:syntax match prekappaLeftBracket /\(%\(loc\|org\|dst\)\)\@<=\[/
:syntax match prekappaRightBracket /\(%\(loc\|org\|dst\)\[\d\+\)\@<=\]/
:syntax match prekappaStatement /^%loc:/
:syntax match prekappaStatement /^%locl:/
:syntax match prekappaStatement /^%locm:/
:syntax match prekappaStatement /^%expand-agent:/
:syntax match prekappaStatement /^%expand-signature:/
:syntax match prekappaStatement /^%expand-rule:/
:syntax match prekappaStatement /^%expand-var:/
:syntax match prekappaStatement /^%expand-obs:/
:syntax match prekappaStatement /^%expand-init:/
:syntax match prekappaStatement /^%expand-mod:/

:syntax match prekappaString /'[^']\+'/ contains=prekappaKeyword

:highlight link prekappaStatement kappaStatement
:highlight link prekappaString kappaString 
:highlight link prekappaKeyword kappaSymbol
:highlight link prekappaLocfield kappaStatement
:highlight link prekappaLabel kappaSymbol
:highlight link prekappaLeftBracket kappaSymbol
:highlight link prekappaRightBracket kappaSymbol
