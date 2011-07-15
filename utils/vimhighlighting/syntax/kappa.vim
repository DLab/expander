:syntax clear
:syntax case match

"Defining 
:syntax keyword kappaStatement do until
:syntax match kappaStatement /^%agent:/
:syntax match kappaStatement /^%var:/
:syntax match kappaStatement /^%plot:/
:syntax match kappaStatement /^%obs:/
:syntax match kappaStatement /^%init:/
:syntax match kappaStatement /^%mod:/
:syntax match kappaStatement /@/
:syntax match kappaStatement /->/
:syntax match kappaStatement /$ADD/
:syntax match kappaStatement /$DEL/
:syntax match kappaStatement /$SNAPSHOT/
:syntax match kappaStatement /$STOP/

:syntax match kappaComment /#.*/

:syntax match kappaString /'[^']\+'/

:syntax match kappaAgent /[a-zA-Z0-9][a-zA-Z0-9_\-]*\((\)\@=\|\.[a-zA-Z0-9][a-zA-Z0-9_\-]*\([^(]*)\)\@=\|\.\.\./
:syntax match kappaAgent /\([a-zA-Z0-9_\-]\)\@<=(/
:syntax match kappaAgent /\([a-zA-Z0-9_\-]([^)]*\)\@<=)/
:syntax match kappaAgent /,\([^(]\+)\)\@=/

:syntax match kappaNumber /\<[-+]\?\d\+\(\.\d*\)\?\([eE][-+]\?\d\+\)\?\>\|\[\(E\|T\|inf\|pi\|emax\|tmax\|true\|false\)\]/

:syntax match kappaLState /!\(\d\+\|_\)\|?\|!/

:syntax match kappaIState /\~\w\+/

:syntax match kappaSymbol /\[\(not\|log\|sin\|cos\|tan\|sqrt\|mod\|exp\|int\)\]\|+\|\*\|\/\|\^\|:\?=\|<\|>\|&&\|||/
:syntax match kappaSymbol /-\(['\[ 0-9]\)\@=/

:highlight link kappaStatement Statement
:highlight link kappaComment Comment
:highlight link kappaString String
:highlight link kappaNumber Number
:highlight link kappaLState Statement
:highlight link kappaIState Identifier
:highlight link kappaAgent Directory
:highlight link kappaSymbol PreProc
