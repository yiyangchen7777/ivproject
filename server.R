# ----------------- SERVER.R -----------------
# 主服务器逻辑入口（连接各子模块）

server <- function(input, output, session) {
  
  # ============= Map 页（暂时空）=============
  observe({
    # 这里之后可以放 map 页逻辑
    # 例如 output$cityMap <- renderLeaflet({...})
  })
  
  # ============= Route 页（暂时空）=============
  observe({
    # 这里之后可以放路线计算逻辑
    # 比如 output$routeInfo <- renderText("Under construction")
  })
  
  # ============= Detail 页 =============
  # 调用你写好的 detail_server 模块
  callModule(detail_server, "detail")
  
  # ============= 通用逻辑（可选）============
  # 例如监测全局事件、消息推送等
  session$onSessionEnded(function() {
    message("Session closed at ", Sys.time())
  })
}
