# 1. 关键步骤：在全新的会话中，加载我们所有的最新代码
devtools::load_all()

# 2. 检查并安装所有可能需要的依赖包
if (!require("e1071", quietly = TRUE)) install.packages("e1071")
if (!require("glmnet", quietly = TRUE)) install.packages("glmnet")
if (!require("ranger", quietly = TRUE)) install.packages("ranger")
if (!require("xgboost", quietly = TRUE)) install.packages("xgboost")

# 3. 加载注册表
registry <- load_model_registry()

# 4. 获取 SVM 配置
svm_config <- get_model_config(registry, "SVM")

# 5. 定义 SVM 的搜索空间 (在干净的环境下，这必须成功)
svm_space <- define_search_space(svm_config)

# 6. 打印搜索空间以进行最终确认
print(svm_space)


# 1. 重新加载我们的软件包，让所有修改生效
devtools::load_all()

# 2. 加载更新后的注册表
registry <- load_model_registry()

# 3. 获取 "LR" (Logistic Regression) 的配置，它被标记为 Tunable = FALSE
lr_config <- get_model_config(registry, "LR")

# 4. 尝试为 LR 定义搜索空间
# 这次我们期待一个清晰的、可控的错误信息，而不是默认的 switch 错误
tryCatch(
  define_search_space(lr_config),
  error = function(e) {
    message("SUCCESS: The robustness test passed!")
    message("Error message was: ", e$message)
  }
)

# 1. 关键步骤：重启R会话后，第一件事就是加载所有最新代码
devtools::load_all()

# 2. 为新中队安装核心依赖包
if (!require("lightgbm", quietly = TRUE)) install.packages("lightgbm")
if (!require("catboost", quietly = TRUE)) {
  # catboost installation can be tricky, this is a common way
  install.packages('catboost', repos = 'https://catboost.github.io/catboost/R-package')
}
if (!require("devtools", quietly = TRUE)) install.packages("devtools")

devtools::install_github('catboost/catboost/catboost/R-package')

# 3. 加载更新后的注册表
registry <- load_model_registry()

# 4. 获取 LGBM 的配置
lgbm_config <- get_model_config(registry, "LGBM")

# 5. 测试为 LGBM 定义搜索空间
lgbm_space <- define_search_space(lgbm_config)

# 6. 打印新的搜索空间，确认新武器已就绪
print(lgbm_space)

# 1. 关键步骤：在全新的会话中，加载我们所有的最新代码
devtools::load_all()

# 2. 检查并安装所有需要的依赖包
if (!require("mboost", quietly = TRUE)) install.packages("mboost")

# 3. 加载我们最终的注册表
registry <- load_model_registry()

# 4. 获取 GAMBOOST 的配置
gamboost_config <- get_model_config(registry, "GAMBOOST")

# 5. 测试为 GAMBOOST 定义搜索空间
gamboost_space <- define_search_space(gamboost_config)

# 6. 打印新的搜索空间，确认最终武器已就绪
print(gamboost_space)

# 检查并安装 testthat (如果需要)
if (!require("testthat", quietly = TRUE)) install.packages("testthat")

# 初始化 testthat 框架
usethis::use_testthat()

usethis::use_test("config_system")

devtools::test()

# This command will create a new repository on your GitHub account
# and push your local project to it.
usethis::use_github()

usethis::create_github_token()

gitcreds::gitcreds_set()

usethis::use_github()

usethis::use_readme_rmd()
