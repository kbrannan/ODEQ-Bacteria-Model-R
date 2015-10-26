@echo off
del be*.out
del directdep*.mut
"C:\Program Files\R\R-3.1.0\bin\x64\Rscript.exe" --vanilla SourceControl\SourceControl.R
xhspfx < hspf.in > null
"C:\Program Files\R\R-3.1.0\bin\x64\Rscript.exe" --vanilla PostProcess\PostProc.R