# Keep It Tidy - Tab Manager
Chrome/Firefox Extension

[Chrome Web Store](https://chrome.google.com/webstore/detail/keep-it-tidy-tab-manager/ncjlgbnopdeldjbdbcpgdepfifhpocip)

[Firefox Add-ons](https://addons.mozilla.org/en-US/firefox/addon/kit/)

Search example: power rangers, superman, batman = (power AND rangers) OR superman OR batman

Press Enter when there is no match to search instead on Google.

If only one tab is selected, pressing Enter will focus this tab.

You can sort all windows by website.

You can select duplicates.

You can close, sort by website, pin/unpin the selected tabs.

You can extract the selection to a new window.

You can move the selection by dragging it to a new position in the left panel. The selected tabs will be inserted before the tab you dropped the selection on.

You can doubleclick a tab in the left panel or click a tab in the right panel to focus this tab.

You can invert the selection, add similar (same website) tabs to the selection.

You can sort the list by last visited or group the tabs by website/window.

Select duplicates does not select the first tab of each family of duplicates ('first' as first in the list).

Matches from search are added to the selection unless you empty the search field.

Selecting a tab from a disabled window will automatically enable this window.

KIT doesn't collect any data.

### Chrome

You can assign a keyboard shortcut to Keep It Tidy:
chrome://extensions/shortcuts


## Compilation

Make sure [elm](https://elm-lang.org/) is in your path then run:

```
./build.sh firefox
./build.sh chrome
```
