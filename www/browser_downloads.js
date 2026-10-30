// Download buttons in the browser (Shinylive) version of the dashboard.
//
// Shinylive serves a download from the R session running in this tab. A
// normal link opens the file URL as a separate request that cannot reach
// that session, so the browser saves a failed ".htm" file instead. Here the
// file is fetched from within the page and saved from memory; it never
// leaves the visitor's computer. In the regular Shiny app this is not needed
// and normal downloads are left unchanged.
(function () {
  if (window.monashBrowserDownloads) return;
  window.monashBrowserDownloads = true;

  var inBrowserR = /\/app_[A-Za-z0-9]+\//.test(window.location.pathname);
  if (!inBrowserR) return;

  function fileName(response, link) {
    var header = response.headers.get('content-disposition') || '';
    var encoded = header.match(/filename\*=(?:UTF-8'')?([^;]+)/i);
    if (encoded) return decodeURIComponent(encoded[1].replace(/"/g, '').trim());
    var plain = header.match(/filename="?([^";]+)"?/i);
    if (plain) return plain[1].trim();
    return link.getAttribute('download') || 'download';
  }

  function notify(text) {
    if (window.Shiny && Shiny.notifications && Shiny.notifications.show) {
      Shiny.notifications.show({ html: text, type: 'error', duration: 8000 });
    } else {
      console.error(text);
    }
  }

  document.addEventListener('click', function (event) {
    var link = event.target.closest && event.target.closest('a.shiny-download-link');
    if (!link) return;
    event.preventDefault();
    event.stopPropagation();
    if (link.classList.contains('disabled') || link.dataset.downloading) return;
    link.dataset.downloading = '1';
    link.setAttribute('aria-busy', 'true');

    fetch(link.href)
      .then(function (response) {
        if (!response.ok) throw new Error('HTTP ' + response.status);
        return response.blob().then(function (blob) {
          return { blob: blob, name: fileName(response, link) };
        });
      })
      .then(function (file) {
        var url = URL.createObjectURL(file.blob);
        var anchor = document.createElement('a');
        anchor.href = url;
        anchor.download = file.name;
        anchor.style.display = 'none';
        document.body.appendChild(anchor);
        anchor.click();
        anchor.remove();
        setTimeout(function () { URL.revokeObjectURL(url); }, 30000);
      })
      .catch(function (error) {
        notify('The download could not be created. Please try again. (' + error.message + ')');
      })
      .finally(function () {
        delete link.dataset.downloading;
        link.removeAttribute('aria-busy');
      });
  }, true);
})();
