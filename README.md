# Keep It Tidy - Tab Manager
Chrome/Firefox Extension

[Chrome Web Store](https://chrome.google.com/webstore/detail/top-tomato/ncjlgbnopdeldjbdbcpgdepfifhpocip)

[Firefox Add-ons](https://addons.mozilla.org/en-US/firefox/addon/top-tomato/)

Previously known as Top Tomato

You can search the tabs by entering keywords into the field.
Example: power rangers, superman, batman = (power AND rangers) OR superman OR batman
Press Enter when there is no match to search for these keywords on Google.

If only one tab is selected, pressing Enter will focus this tab.

You can close, sort by website, pin/unpin the selected tabs.
You can extract the selection to a new window.
You can move the selection by dragging it to a new position in the left panel. The selected tabs will be inserted before the tab you dropped the selection on.

You can doubleclick a tab in the left panel to focus this tab.

You can invert the selection, add similar (same website) tabs to the selection or select duplicates.

You can sort the list by last visited or group the tabs by website/window.

Select duplicates does not select the first tab of each family of duplicates ('first' as first in the list).

Matches from search are added to the selection unless you empty the search field.

Selecting a tab from a disabled window will automatically enable this window.

### Chrome

You can assign a keyboard shortcut to Keep It Tidy:
chrome://extensions/shortcuts


## Compilation

Make sure [elm](https://elm-lang.org/) is in your path then run:

```
./build.sh firefox
./build.sh chrome
```
