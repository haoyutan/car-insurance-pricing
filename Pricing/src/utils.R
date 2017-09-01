utils.recordPlot.prepare <-
function(tmppng = tempfile(tmpdir = '/tmp', fileext = '.png'))
{
  png(tmppng)
  dev.control(displaylist = 'enable')
}


utils.recordPlot.done <-
function()
{
  dev.off()
}
