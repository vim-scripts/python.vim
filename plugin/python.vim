" -*- vim -*-
" FILE: python.vim
" LAST MODIFICATION: 2003/07/25 19:00
" (C) Copyright 2001-2003 Mikael Berthe <mikael.b@netcourrier.com>
" Version: 1.7

" USAGE:
"
" Just source this script when editing Python files.
" Example: au FileType python source ~me/.vim/scripts/python.vim
" You can set the global variable "g:py_select_leading_comments" to 0
" if you don't want to select comments preceding a declaration (these
" are usually the description of the function/class).
" You can set the global variable "g:py_select_trailing_comments" to 0
" if you don't want to select comments at the end of a function/class.
" If these variables are not defined, both leading and trailing comments
" are selected.
" Example: (in your .vimrc) "let g:py_select_leading_comments = 0"
" You may want to take a look at the 'shiftwidth' option for the
" shift commands...
"
" REQUIREMENTS:
" vim (>= 6)
"
" Shortcuts:
"   [[      -- Jump to beginning of block
"   ]]      -- Jump to end of block
"   ]v      -- Select (Visual Line Mode) block
"   ]<      -- Shift block to left
"   ]>      -- Shift block to right
"   ]c      -- Select current/previous class
"   ]f      -- Select current/previous function
"   ]<up>   -- Jump to previous line with the same/lower indentation
"   ]<down> -- Jump to next line with the same/lower indentation


map  [[   :PBoB<CR>
vmap [[   :<C-U>PBoB<CR>m'gv``
map  ]]   :PEoB<CR>
vmap ]]   :<C-U>PEoB<CR>m'gv``

map  ]v   [[V]]
map  ]<   [[V]]<
vmap ]<   <
map  ]>   [[V]]>
vmap ]>   >

map  ]c   :call PythonSelectObject("class")<CR>
map  ]f   :call PythonSelectObject("function")<CR>

map  ]<up>    :call PythonNextLine(-1)<CR>
map  ]<down>  :call PythonNextLine(1)<CR>
" You may prefer use <s-up> and <s-down>... :-)



" Menu entries
nmenu <silent> &Python.Update\ IM-Python\ Menu 
    \:call UpdateMenu()<CR>
nmenu &Python.-Sep1- :
nmenu <silent> &Python.Beginning\ of\ Block<Tab>[[ 
    \[[
nmenu <silent> &Python.End\ of\ Block<Tab>]] 
    \]]
nmenu &Python.-Sep2- :
nmenu <silent> &Python.Shift\ Block\ Left<Tab>]< 
    \]<
vmenu <silent> &Python.Shift\ Block\ Left<Tab>]< 
    \]<
nmenu <silent> &Python.Shift\ Block\ Right<Tab>]> 
    \]>
vmenu <silent> &Python.Shift\ Block\ Right<Tab>]> 
    \]>
nmenu &Python.-Sep3- :
vmenu <silent> &Python.Comment\ Selection 
    \:call PythonCommentSelection()<CR>
nmenu <silent> &Python.Comment\ Selection 
    \:call PythonCommentSelection()<CR>
vmenu <silent> &Python.Uncomment\ Selection 
    \:call PythonUncommentSelection()<CR>
nmenu <silent> &Python.Uncomment\ Selection 
    \:call PythonUncommentSelection()<CR>
nmenu &Python.-Sep4- :
nmenu <silent> &Python.Previous\ Class 
    \:call PythonDec("class", -1)<CR>
nmenu <silent> &Python.Next\ Class 
    \:call PythonDec("class", 1)<CR>
nmenu <silent> &Python.Previous\ Function 
    \:call PythonDec("function", -1)<CR>
nmenu <silent> &Python.Next\ Function 
    \:call PythonDec("function", 1)<CR>
nmenu &Python.-Sep5- :
nmenu <silent> &Python.Select\ Block<Tab>]v 
    \]v
nmenu <silent> &Python.Select\ Function<Tab>]f 
    \]f
nmenu <silent> &Python.Select\ Class<Tab>]c 
    \]c
nmenu &Python.-Sep6- :
nmenu <silent> &Python.Previous\ Line\ wrt\ indent<Tab>]<up> 
    \]<up>
nmenu <silent> &Python.Next\ Line\ wrt\ indent<Tab>]<down> 
    \]<down>


:com! PBoB execute "normal ".PythonBoB(line('.'), -1, 1)."G"
:com! PEoB execute "normal ".PythonBoB(line('.'), 1, 1)."G"
:com! UpdateMenu call UpdateMenu()


" Go to a block boundary (-1: previous, 1: next)
" If force_sel_comments is true, 'g:py_select_trailing_comments' is ignored
function! PythonBoB(line, direction, force_sel_comments)
  let ln = a:line
  let ind = indent(ln)
  let mark = ln
  let indent_valid = strlen(getline(ln))
  let ln = ln + a:direction
  if (a:direction == 1) && (!a:force_sel_comments) && 
      \ exists("g:py_select_trailing_comments") && 
      \ (!g:py_select_trailing_comments)
    let sel_comments = 0
  else
    let sel_comments = 1
  endif

  while((ln >= 1) && (ln <= line('$')))
    if  (sel_comments) || (match(getline(ln), "^\\s*#") == -1)
      if (!indent_valid)
        let indent_valid = strlen(getline(ln))
        let ind = indent(ln)
        let mark = ln
      else
        if (strlen(getline(ln)))
          if (indent(ln) < ind)
            break
          endif
          let mark = ln
        endif
      endif
    endif
    let ln = ln + a:direction
  endwhile

  return mark
endfunction


" Go to previous (-1) or next (1) class/function definition
function! PythonDec(obj, direction)
  if (a:obj == "class")
    let objregexp = "^\\s*class\\s\\+[a-zA-Z0-9_]\\+"
        \ . "\\s*\\((\\([a-zA-Z0-9_,. \\t\\n]\\)*)\\)\\=\\s*:"
  else
    let objregexp = "^\\s*def\\s\\+[a-zA-Z0-9_]\\+\\s*(\\_[^:#]*)\\s*:"
  endif
  let flag = "W"
  if (a:direction == -1)
    let flag = flag."b"
  endif
  let res = search(objregexp, flag)
endfunction


" Comment out selected lines
" commentString is inserted in non-empty lines, and should be aligned with
" the block
function! PythonCommentSelection()  range
  let commentString = "#"
  let cl = a:firstline
  let ind = 1000    " I hope nobody use so long lines! :)

  " Look for smallest indent
  while (cl <= a:lastline)
    if strlen(getline(cl))
      let cind = indent(cl)
      let ind = ((ind < cind) ? ind : cind)
    endif
    let cl = cl + 1
  endwhile
  if (ind == 1000)
    let ind = 1
  else
    let ind = ind + 1
  endif

  let cl = a:firstline
  execute ":".cl
  " Insert commentString in each non-empty line, in column ind
  while (cl <= a:lastline)
    if strlen(getline(cl))
      execute "normal ".ind."|i".commentString
    endif
    execute "normal \<Down>"
    let cl = cl + 1
  endwhile
endfunction

" Uncomment selected lines
function! PythonUncommentSelection()  range
  " commentString could be different than the one from CommentSelection()
  " For example, this could be "# \\="
  let commentString = "#"
  let cl = a:firstline
  while (cl <= a:lastline)
    let ul = substitute(getline(cl),
             \"\\(\\s*\\)".commentString."\\(.*\\)$", "\\1\\2", "")
    call setline(cl, ul)
    let cl = cl + 1
  endwhile
endfunction


" Select an object ("class"/"function")
function! PythonSelectObject(obj)
  " Go to the object declaration
  normal $
  call PythonDec(a:obj, -1)
  let beg = line('.')

  if !exists("g:py_select_leading_comments") || (g:py_select_leading_comments)
    let decind = indent(beg)
    let cl = beg
    while (cl>1)
      let cl = cl - 1
      if (indent(cl) == decind) && (getline(cl)[decind] == "#")
        let beg = cl
      else
        break
      endif
    endwhile
  endif

  if (a:obj == "class")
    let eod = "\\(^\\s*class\\s\\+[a-zA-Z0-9_]\\+\\s*"
            \ . "\\((\\([a-zA-Z0-9_,. \\t\\n]\\)*)\\)\\=\\s*\\)\\@<=:"
  else
   let eod = "\\(^\\s*def\\s\\+[a-zA-Z0-9_]\\+\\s*(\\_[^:#]*)\\s*\\)\\@<=:"
  endif
  " Look for the end of the declaration (not always the same line!)
  call search(eod, "")

  " Is it a one-line definition?
  if match(getline('.'), "^\\s*\\(#.*\\)\\=$", col('.')) == -1
    let cl = line('.')
    execute ":".beg
    execute "normal V".cl."G"
  else
    " Select the whole block
    execute "normal \<Down>"
    let cl = line('.')
    execute ":".beg
    execute "normal V".PythonBoB(cl, 1, 0)."G"
  endif
endfunction


" Jump to the next line with the same (or lower) indentation
" Useful for moving between "if" and "else", for example.
function! PythonNextLine(direction)
  let ln = line('.')
  let ind = indent(ln)
  let indent_valid = strlen(getline(ln))
  let ln = ln + a:direction

  while((ln >= 1) && (ln <= line('$')))
    if (!indent_valid) && strlen(getline(ln)) 
        break
    else
      if (strlen(getline(ln)))
        if (indent(ln) <= ind)
          break
        endif
      endif
    endif
    let ln = ln + a:direction
  endwhile

  execute "normal ".ln."G"
endfunction


" Update the IM-Python menu, that holds Classes and Functions
function! UpdateMenu()
  let restore_fe = &foldenable
  set nofoldenable
  let cline=line('.')
  call MakeClassStructure ()
  call MakeFuncStructure ()
  execute "normal ".cline."Gzz"
  let &foldenable = restore_fe
endfunction

" Make a menu that holds all of the classes
function! MakeClassStructure () 
  norm mpgg0
  while line(".") <= line("$")
    if match ( getline("."), '^\s*class\s\+' ) != -1
      norm ^w"nyw
      let name=@n
      "exe 'menu IM-Python.classes.'.name.' '.line(".").'gg15zo'
      exe 'menu IM-Python.classes.'.name.' :call <SID>JumpToAndUnfold('.line(".").')<CR>'
    endif
    if line(".") == line("$")
      return
    endif
    norm j
  endwhile
  norm 'p
endfunction

" Make a menu that holds all of the function definitions
function! MakeFuncStructure () 
  norm mpgg0
  while line(".") <= line("$")
    if match ( getline("."), '^\s*def\s\+' ) != -1
      norm ^w"nyw
      let name=@n
      "exe 'menu IM-Python.functions.'.name.' '.line(".").'gg15zo'
      exe 'menu IM-Python.functions.'.name.' :call <SID>JumpToAndUnfold('.line(".").')<CR>'
    endif
    if line(".") == line("$")
      return
    endif
    norm j
  endwhile
  norm 'p
endfunction

function! s:JumpToAndUnfold(line)
  " Go to the right line
  execute 'normal '.a:line.'gg'
  " Check to see if we are in a fold
  let lvl = foldlevel(a:line)
  if lvl != 0
    " and if so, then expand the fold out, other wise, ignore this part.
    execute 'normal 15zo'
  endif
endfunction

"" This one will work only on vim 6.2 because of the try/catch expressions.
" function! s:JumpToAndUnfoldWithExceptions(line)
"  try 
"    execute 'normal '.a:line.'gg15zo'
"  catch /^Vim\((\a\+)\)\=:E490:/
"    " Do nothing, just consume the error
"  endtry
"endfunction


" vim:set et sts=2 sw=2:

