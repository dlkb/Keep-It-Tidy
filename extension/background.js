var MAX_LENGTH = 333; // max length of array used to keep track of the browsing history
var visited = []; // tabIds of the last visited tabs
var timeout;

function pushTo(arr, item) {
  arr.unshift(item);
  if (arr.length>MAX_LENGTH) {
    arr.pop();
  }
};

// newTree notifies the popup that the tree has been updated
function newTree() {
  if (timeout) { // cancel the previous call to getWindows if close to the current one
    clearTimeout(timeout);
  }
  timeout = setTimeout(function() {
    getWindows(function(wins) {
      windows = JSON.stringify(wins);
      chrome.runtime.sendMessage({
        "task": "newTree", 
        "windows": windows
      });
    });
  }, 1);
}

function getWindows(callback) {
  chrome.windows.getAll({"populate":true, "windowTypes":["normal"]}, function(wins) {
    callback(wins);
  });
}

chrome.runtime.onMessage.addListener(function(message, sender, sendResponse) {
  switch (message.task) {
  case "init":
    getWindows(function(wins) {
      windows = JSON.stringify(wins);
      sendResponse({"windows": windows, "visited": visited});
    });
    return true;
  case "createTab":
    chrome.tabs.create({"windowId": message.windowId});
    return;
  case "createWindow":
    chrome.windows.create();
    return;
  case "removeTabs":
    chrome.tabs.remove(message.tabIds);
    return;
  case "extractTabs":
    var tabIds = message.tabIds;
    var isTabPinned = {};
    for (var i=0, tabId; tabId=tabIds[i]; i++) {
      (function(tabId) {
        chrome.tabs.get(tabId, function(tab) {
          isTabPinned[tabId] = tab.pinned;
        });
      })(tabId);
    }
    var firstTabId = tabIds.shift(); 
    chrome.windows.create({"tabId":firstTabId}, function(win) {
      chrome.tabs.update(firstTabId, {"pinned":isTabPinned[firstTabId]});
      chrome.tabs.move(tabIds, {"windowId":win.id, "index":1}, function() {
        var count = 0;
        for (var i=0, tabId; tabId=tabIds[i]; i++) {
          (function(tabId) {
            chrome.tabs.update(tabId, {"pinned":isTabPinned[tabId]});
          })(tabId);
        }
      });
    });
    return;
  case "pinTabs":
    var tabIds = message.tabIds;
    var count = 0;
    for (var i=0, tabId; tabId=tabIds[i]; i++) {
      (function(tabId) {
        chrome.tabs.get(tabId, function(tab) {
          chrome.tabs.update(tabId, {"pinned": !tab.pinned});
        });
      })(tabId);
    }
    return;
  case "sortTabs":
    var tabIds = message.tabIds;
    var urls = {}; // tabId1 : url, ...
    f = function(callback) {
      var count = 0;
      for (var i=0, tabId; tabId=tabIds[i]; i++) {
        (function(tabId) {
          count++;
          chrome.tabs.get(tabId, function(tab) {
            urls[tabId] = tab.url;
            if (count == tabIds.length) {
              callback();
            }
          });
        })(tabId);
      }
    }
    callback = function() {
      tabIds.sort(function(tabId1, tabId2) {
        if (urls[tabId1] < urls[tabId2]) {
          return -1;
        }
        if (urls[tabId1] > urls[tabId2]) {
          return 1;
        }
      });
      chrome.tabs.move(tabIds, {"index": -1}); // defaults to the window the tab is currently in
    }
    f(callback);
    return;
  case "focusWindow":
    chrome.windows.update(message.windowId, {"focused":true});
    return
  case "focusTab":
    chrome.tabs.update(message.tabId, {"active":true}, function(tab) {
      chrome.windows.update(tab.windowId, {"focused":true});
    });
    return
  case "moveTabs":
    chrome.tabs.move(message.tabIds, {"index": message.index, "windowId": message.windowId});
    return;
  }
});

chrome.tabs.onUpdated.addListener(function(tabId, changeInfo) {
  if (changeInfo.hasOwnProperty("pinned")) {
    newTree();
  }
});

chrome.tabs.onCreated.addListener(function(tab) {
  newTree();
});

chrome.tabs.onMoved.addListener(function(tabId, moveInfo) {
  newTree();
});

chrome.tabs.onDetached.addListener(function(tabId, moveInfo) {
  newTree();
});

chrome.tabs.onRemoved.addListener(function(tabId, removeInfo) {
  newTree();
});

chrome.tabs.onActivated.addListener(function(activeInfo) {
  pushTo(visited, activeInfo.tabId);
});

chrome.windows.onFocusChanged.addListener(function(windowId) {
  if (windowId===chrome.windows.WINDOW_ID_NONE) {
    return;
  }
  chrome.tabs.query({"active":true, "currentWindow":true}, function(tabs) {
    if (tabs[0]) {
      pushTo(visited, tabs[0].id);
    }
  });
});

var UPDATE_FROM_102 = "Top Tomato is not ugly anymore. Check the Chrome Web Store for more info.";

chrome.runtime.onInstalled.addListener(function(details) {
  if (details.reason=="install") { // if first installation
  // Set default values for options
  } else if (details.reason=="update") { // if update of the extension
  // Say something to the user.
    if (details.previousVersion=="1.0.2") {
      var options = {
        type: "basic",
        title: "Update",
        message: UPDATE_FROM_102,
        contextMessage: "- Damien",
        iconUrl: "img/128.png"
      }
      chrome.notifications.create("update", options, function(){});
    }
  }
});
