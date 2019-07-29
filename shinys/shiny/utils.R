# utils functions
html_tpl_dashboard_box <- function(fat_text,small_text,icon,bg_color,tooltip="")
{
  tpl  = '
<div class="panel panel-primary">
	<div class="panel-heading" style="background-color:@{bg_color};border-color:@{bg_color};">
		<div class="row">
			<div class="col-xs-2 icon-hide-responsive">
				<i class="fa @{icon} fa-4x"></i>
			</div>
			<div class="col-xs-10 text-center">
				<div class="huge nowrap"   >@{fat_text}</div>
				<div class="nowrap" data-toggle="tooltip" data-placement="bottom" title="@{tooltip}" >@{small_text}</div>
			</div>
		</div>
	</div>

  <script>$(\'[data-toggle="tooltip"]\').tooltip()</script>
</div>
  '
  return(HTML(GetoptLong::qq(tpl,collapse=FALSE)))
}

html_tpl_dashboard_box_with_sup <- function(fat_text,small_text,icon,bg_color,tooltip,sup,sup_tooltip="")
{
  tpl = '@{fat_text} <sup class="dashboard-sup" data-toggle="tooltip" data-placement="right" title="@{sup_tooltip}">@{sup}</sup>'
  return(html_tpl_dashboard_box(qq(tpl,collapse=F),small_text,icon,bg_color,tooltip))
}

color_pal = c('#d7191c','#fdae61','#97C561')

dashboard_bg_color <- function(n,orange,green)
{
  return(color_pal[[
      ifelse(n<orange,1,
        ifelse(n<green,2,3)
        )]])
}

format_percent_number <-function(n,dec_num=1)
{
  return(sprintf(paste0(ifelse(n>=0,'+%.','%.'),
                        dec_num,
                        'f %%'),n))
}
