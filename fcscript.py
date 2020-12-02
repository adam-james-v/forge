import FreeCAD
import importCSG
import Import

App.newDocument("a")
doc = FreeCAD.getDocument("a")

importCSG.insert(u"/Users/adam/dev/forge/out/scadout.csg", "a")

__objs__ = doc.RootObjects
Import.export(__objs__, u"/Users/adam/dev/forge/out/fcout.step")

del __objs__
