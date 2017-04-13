"=============================================================================
" Filename: autoload/lightline/colorscheme/mashup.vim
" Author: Treia
" License: MIT License
" Last Change: 2013/09/07 12:23:38.
" =============================================================================
let s:back= [ '#171717' , '234' ] "Background
let s:base03 = [ '#404040' , '8' ] "8
let s:base02 = [ '#202020' , '0' ] "0
let s:base01 = [ '#b8ca4b' , '10' ] "10
let s:base00 = [ '#c7a551' , '11' ] "11
let s:base0 = [ '#95b9de' , '12' ] "12
let s:base1 = [ '#9fa590' , '14' ] "14
let s:base2 = [ '#b2a191' , '7' ] "7
let s:base3 = [ '#e8d0c3' , '15' ] "15
let s:yellow = [ '#bf7a29' , '3' ] "3
let s:orange = [ '#ff6c5f' , '9' ] "9
let s:red = [ '#bf3f34' , '1' ] "1
let s:magenta = [ '#75507b' , '5' ] "5
let s:violet = [ '#ad7fa8' , '13' ] "13
let s:blue = [ '#627a92' , '4' ] "4
let s:cyan = [ '#757978' , '6' ] "6
let s:green = [ '#707d22' , '2' ] "2

let s:p = {'normal': {}, 'insert': {}, 'visual': {}, 'replace': {}, 'tabline': {}, 'inactive': {}}
let s:p.normal.left = [ [ s:base2, s:red, "bold" ], [ s:magenta, s:base02 ] ]
let s:p.normal.right = [ [ s:base2, s:red, "bold" ], [ s:magenta, s:base02 ] ]
let s:p.normal.middle = [ [ s:red, s:back, "italic" ] ]
let s:p.insert.left = [ [ s:base2, s:magenta, "bold" ], [ s:base02, s:violet ] ]
let s:p.visual.left = [ [ s:base2, s:green, "bold" ], [ s:base02, s:base01 ] ]
let s:p.replace.left = [ [ s:base2, s:blue, "bold" ], [ s:base02, s:base0 ] ]
let s:p.normal.error = [ [ s:back, s:yellow, ] ]
let s:p.normal.warning = [ [ s:base02, s:yellow ] ]
let s:p.tabline.left = [ [ s:magenta, s:red ] ]
let s:p.tabline.middle = [ [ s:yellow, s:green ] ]
let s:p.tabline.tabsel = [ [ s:base2, s:back ] ]
let s:p.tabline.right = copy(s:p.normal.right)
let s:p.inactive.middle = [ [ s:back, s:back ] ]

let g:lightline#colorscheme#Mashup#palette = lightline#colorscheme#flatten(s:p)
