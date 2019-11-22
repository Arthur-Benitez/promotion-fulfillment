
require(shiny)
require(shinydashboard)

header <- dashboardHeader(
  title = tags$div(
    tags$div(
      id = 'app-logo-container',
      tags$img(src = 'spark-logo.png', id = 'app-logo')
    ),
    tags$div(
      id = 'app-name-container',
      tags$p(id = 'app-name', lang$app_name)
      # tags$p(id = 'app-description', 'XXXXXXX')
    )
  ),
  tags$li(
    class = 'dropdown header-icons',
    tags$div(
      title = lang$help_title,
      actionButton('help', '', icon = icon('question-circle'), class = 'header-icon')
    ),
    tags$div(
      title = lang$logout_timeout_info,
      class = 'header-text',
      logoutUI('logout')
    ),
    uiOutput('user_level_icon')
  )
)

sidebar <- dashboardSidebar(
  collapsed = TRUE,
  uiOutput('sidebar'),
  column(
    icon('empire'),
    gl$app_version_text,
    align = 'right',
    width = 12,
    style = 'position: absolute; bottom: 0;'
  )
)

body <- dashboardBody(
  useShinyjs(),
  useShinyalert(),
  tags$head(
    tags$link(rel = 'stylesheet', type = 'text/css', href = sprintf('theme.css?%s', Sys.time())),
    tags$link(rel = 'stylesheet', type = 'text/css', href = sprintf('season/season.css?%s', Sys.time())),
    # includeCSS('www/theme.css'), ## Alternativa que mete el CSS al HTML, por si se necesita
    tags$link(rel = 'stylesheet', href = 'https://fonts.googleapis.com/css?family=Bree+Serif|Coiny'),
    HTML(
      "<script type='text/javascript'>
        const orchestrator = function (e) {
        document.cookie = name + '=; expires=Thu, 01 Jan 1970 00:00:01 GMT;';
	        var loop = setInterval(
	          function () {
            	let userName = e.data.userName;
  	          setCookie('CompassUserName',userName,1);
              parent.window.postMessage( { name : 'stopLoading' }, '*');
            	clearInterval(loop);
        	  }, 750
          );
	      }
        const setCookie  = function(name,value,days) {
	        let expires = '';
        	if (days) {
	          let date = new Date();
            date.setTime(date.getTime() + (10*60*1000));
            expires = ';expires=' + date.toUTCString();
        	}
          document.cookie = name + '=' + (value || '')  + expires + '; path=/';
        }
        if (window.addEventListener)
          window.addEventListener('message', orchestrator, false);
        else
	        window.attachEvent('onmessage', orchestrator);
	    </script>"
    )
  ),
  uiOutput('body')
)

dashboardPage(
  title = lang$app_name,
  header = header,
  sidebar = sidebar,
  body = body
)
