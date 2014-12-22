import Graphics.UI.WX

buildGUI = do
  f <- frame [ text := "Hello" ]

  controls <- panel f []
  ctext <- staticText controls [ text := "Foo" ]
  butn <- button controls [text := "Remove the Foo"]        -- I've added a button to remove Foo
  set controls [ layout := row 0 [margin 5 (widget ctext),
                                  margin 5 (widget butn) ]]

  set f [ layout := widget controls ]

  set butn [on command := do
      set ctext [visible := False]                          -- so ctext doesn't show
      --set controls [layout := margin 5 (widget butn) ]
            ]     -- so ctext doesn't take up space
  return ()

main = start buildGUI
