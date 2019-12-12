# Top Tomato
Chrome/Firefox Extension - Tab Manager

[Chrome Web Store](https://chrome.google.com/webstore/detail/top-tomato/ncjlgbnopdeldjbdbcpgdepfifhpocip)

[Firefox Add-ons](https://addons.mozilla.org/en-US/firefox/addon/top-tomato/)


Use Top Tomato to group, move, delete, sort, pin tabs.

You can search the selection by entering keywords into the field.
Example: power rangers, superman, batman = (power AND rangers) OR superman OR batman

If only one tab is selected, press Enter to focus this tab.

Actions like "extract" or "invert selection" apply to the whole selection unless there are checked tabs, in which case they apply to the subselection.

You can move tabs by dragging the "n x Tab" icon in the toolbar to a new position in the left panel. The selected tabs will be inserted before the tab you dropped the selection on.

You can doubleclick a tab in the left panel to focus this tab.

You can doubleclick a window in the left panel to focus this window.


### Chrome

Works in incognito mode if enabled by the user (see extension details).

You can assign a keyboard shortcut to Top Tomato: chrome://extensions/shortcuts


### Firefox

Drag and drop doesn't work on Sway.


## Compilation

Make sure [elm](https://elm-lang.org/) is in your path then run:

```
./build.sh firefox
./build.sh chrome
```
