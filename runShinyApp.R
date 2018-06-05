message('library paths:\n', paste('... ', .libPaths(), sep='', collapse='\n'))

chrome.portable = file.path(getwd(),
                            'app-files/GoogleChromePortable/App/Chrome-bin/chrome.exe')

launch.browser = function(appUrl, browser.path=chrome.portable) {
    message('Browser path: ', browser.path)
    shell(sprintf('"%s" --app=%s', browser.path, appUrl))
}

shiny::runApp('./app-files/shiny/', launch.browser=launch.browser)