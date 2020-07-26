function onLoad() {
    var hidePanelCheckbox = document.getElementById('alwaysOnPanel');
    var hintsCheckbox = document.getElementById('hints');

    browser.storage.sync.get(["alwaysOnPanel", "hints"], function (prefs) {
        hidePanelCheckbox.checked = !prefs.alwaysOnPanel;
        hintsCheckbox.checked = prefs.hints;

        hidePanelCheckbox.addEventListener("change", function () {
            browser.storage.sync.set({
                "alwaysOnPanel": !hidePanelCheckbox.checked,
            });
        });
        hintsCheckbox.addEventListener("change", function () {
            browser.storage.sync.set({
                "hints": hintsCheckbox.checked
            });
        });
    });
}
document.addEventListener('DOMContentLoaded', onLoad);