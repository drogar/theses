\begin{code}
module InstructionNames where

iname_newint = "QMove"
iname_newqbit = "QLoad" 
iname_newdata = "QCons"
iname_discard = "QDiscard"
iname_bind = "QBind"
iname_unbind = "QUnbind"
iname_transform = "QApply"
iname_measure = "Measure"
iname_split = "Split"
iname_use = "Use" 
iname_endqc = "EndQC" 
iname_jump = "Jump"
iname_cjump = "CondJump"
iname_call = "Call"
iname_return = "Return"
iname_pullup = "QPullup"
iname_delete = "QDelete"
iname_rename = "QName"
iname_loadi = "CLoad"
iname_cget = "CGet"
iname_cput = "CPut"
iname_coperation = "CApply"
iname_cpop = "CPop"
iname_noop = "NoOp"
iname_zerostack = "ZeroStack"
iname_popcontrol = "UnCtrl"
iname_pushcontrol = "AddCtrl"
iname_controlit = "QCtrl"

\end{code}

\begin{code}

inamepart_put = "put"
inamepart_combine = "combine"
inamepart_controlled = "Controlled"
inamepart_Had = "Hadamard"
inamepart_Not = "NOT"
inamepart_V = "'V'"
inamepart_W = "'W'"
inamepart_Toff = "Toffoli"
inamepart_X = "'X'"
inamepart_rotate = "Rotate"

glue2 :: String -> String -> String
glue2 first second = "    "++ first ++ " " ++ second

glue3 :: String -> String -> String -> String
glue3 first second third = "    "++ first ++ " " ++ second++ " " ++ third

assem_directive_startproc :: String -> String
assem_directive_startproc nm = nm ++ "   Start"

assem_directive_endproc :: String
assem_directive_endproc = "   EndProc"

assem_directive_starttrns :: String -> String
assem_directive_starttrns nm = nm ++ "   Trans"

assem_directive_endtrns :: String
assem_directive_endtrns = "   EndTrans"

\end{code}
