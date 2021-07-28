{
    application, test,
    [
        %% 应用程序的描述
        {description, "This is game server."},

        %% 应用程序的版本
        {vsn, "1.0.0"},

        %% 应用程序启动时调用模块列表
        {modules, [test]},

        %% 注意这个配置节是指定当前应用程序依赖哪些应用程序，类似Windows服务的依赖关系
        {applications, [kernel, stdlib]},

        %% 启动application的时候回调模块文件名称和Module:start/2函数的参数
        {mod, {test, []}},

        %% 应用程序的注册名称
        {registered, [test]},

        %% 开启阶段调用
        {start_phases, []},

        %% 参数变量配置，以key-value的形式组织配置数据，可以用application:get_env/2读取。
        {env, []
        }
    ]
}.