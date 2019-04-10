const MAX_LENGTH = 333; // max length of array used to keep track of the browsing history
var visited = []; // tabIds of the last visited tabs
var timeout;
var popupIsActive = false;

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
  if (!popupIsActive) { // Don't send anything if popup isn't active
    return;
  }
  timeout = setTimeout(function() {
    getWindows(function(wins) {
      windows = JSON.stringify(wins);
      browser.runtime.sendMessage({
        "task": "newTree", 
        "windows": windows
      });
    });
  }, 100);
}

function getWindows(callback) {
  browser.windows.getAll({"populate":true, "windowTypes":["normal"]}, function(wins) {
    callback(wins);
  });
}

browser.runtime.onMessage.addListener(function(message, sender, sendResponse) {
  switch (message.task) {
  case "init":
    getWindows(function(wins) {
      popupIsActive = true;
      windows = JSON.stringify(wins);
      sendResponse({"windows": windows, "visited": visited});
    });
    return true;
  case "createTab":
    browser.tabs.create({"windowId": message.windowId});
    return;
  case "createWindow":
    browser.windows.create();
    return;
  case "removeTabs":
    browser.tabs.remove(message.tabIds);
    return;
  case "extractTabs":
    var tabIds = message.tabIds;
    var isTabPinned = {};
    for (let tabId of tabIds) {
      browser.tabs.get(tabId, function(tab) {
        isTabPinned[tabId] = tab.pinned;
      });
    }
    var firstTabId = tabIds.shift(); 
    browser.windows.create({"tabId":firstTabId}, function(win) {
      browser.tabs.update(firstTabId, {"pinned":isTabPinned[firstTabId]});
      browser.tabs.move(tabIds, {"windowId":win.id, "index":1}, function() {
        for (let tabId of tabIds) {
          browser.tabs.update(tabId, {"pinned":isTabPinned[tabId]});
        }
      });
    });
    return;
  case "pinTabs":
    var tabIds = message.tabIds;
    var count = 0;
    for (let tabId of tabIds) {
      browser.tabs.get(tabId, function(tab) {
        browser.tabs.update(tabId, {"pinned": !tab.pinned});
      });
    }
    return;
  case "sortTabs":
    var tabIds = message.tabIds;
    var urls = {}; // tabId1 : url, ...
    var titles = {}; // tabId1 : title, ...
    var count = 0;
    for (let tabId of tabIds) {
      browser.tabs.get(tabId, function(tab) {
        count++;
        urls[tabId] = tab.url;
        titles[tabId] = tab.title;
        if (count == tabIds.length) {
          tabIds.sort(function(tabId1, tabId2) {  // sort by concatenation of url and title
            if (urls[tabId1]+titles[tabId1] < urls[tabId2]+titles[tabId2]) {
              return -1;
            }
            if (urls[tabId1]+titles[tabId1] > urls[tabId2]+titles[tabId2]) {
              return 1;
            }
          });
          browser.tabs.move(tabIds, {"index": -1}); // defaults to the window the tab is currently in
        }
      });
    }
    return;
  case "focusWindow":
    browser.windows.update(message.windowId, {"focused":true});
    return;
  case "focusTab":
    browser.tabs.update(message.tabId, {"active":true}, function(tab) {
      browser.windows.update(tab.windowId, {"focused":true});
    });
    return;
  case "moveTabs":
    browser.tabs.move(message.tabIds, {"index": message.index, "windowId": message.windowId});
    return;
  case "removeDuplicates":
    var tabIds = message.tabIds;
    var toRemove = [];
    var existingUrls = [];
    var count = 0;
    for (let tabId of tabIds) {
      browser.tabs.get(tabId, function(tab) {
        count++;
        if (existingUrls.includes(tab.url)) {
          toRemove.push(tabId);
        } else {
          existingUrls.push(tab.url);
        }
        if (count==tabIds.length) {
          browser.tabs.remove(toRemove);
        }
      });
    }
    return;
  case "popupInactive":
    popupIsActive = false;
    return;
  }
});

browser.tabs.onUpdated.addListener(function(tabId, changeInfo) {
  if (changeInfo.hasOwnProperty("pinned")) {
    newTree();
  }
});

browser.tabs.onCreated.addListener(function(tab) {
  newTree();
});

browser.tabs.onMoved.addListener(function(tabId, moveInfo) {
  newTree();
});

browser.tabs.onDetached.addListener(function(tabId, moveInfo) {
  newTree();
});

browser.tabs.onRemoved.addListener(function(tabId, removeInfo) {
  newTree();
});

browser.tabs.onActivated.addListener(function(activeInfo) {
  pushTo(visited, activeInfo.tabId);
});

browser.windows.onFocusChanged.addListener(function(windowId) {
  if (windowId===browser.windows.WINDOW_ID_NONE) {
    return;
  }
  browser.tabs.query({"active":true, "currentWindow":true}, function(tabs) {
    if (tabs[0]) {
      pushTo(visited, tabs[0].id);
    }
  });
});

var UPDATE_FROM_216 = "New feature: you can remove duplicates. Also TT is a bit faster.";

browser.runtime.onInstalled.addListener(function(details) {
  if (details.reason=="install") { // if first installation
  // Set default values for options
  } else if (details.reason=="update") { // if update of the extension
  // Say something to the user.
    if (details.previousVersion=="2.1.6") {
      var options = {
        type: "basic",
        title: "Update",
        message: UPDATE_FROM_216,
        contextMessage: "- Damien",
        iconUrl: "img/128.png"
      }
      browser.notifications.create("update", options, function(){});
    }
  }
});
