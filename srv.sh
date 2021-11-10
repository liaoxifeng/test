#!/bin/bash
#-------------------------------------------------
# 服务端相关开发工具
#-------------------------------------------------

# 检测某个函数是否已经定义
is_fun_exists(){
    declare -F "$1" > /dev/null; return $?
}

# 帮助输出
fun_help(){
    echo "sh srv.sh srv make"
    echo "sh srv.sh srv stop"
    echo "sh srv.sh srv start"
}

# 编译代码
fun_srv_make(){
    if [ ! -d ebin ]; then
    	mkdir ebin
    fi
    cd apps
    ./complie.sh
    cd ..
    erl -pa ebin -noshell -s make all -s init stop
}

# 启动服务器
fun_srv_start(){
    echo $@
    cd config
    erl -name feng@192.168.31.13 -setcookie test -config test -pa ../ebin -s test start -extra zone
}

# 关闭服务器
fun_srv_stop(){
    echo $@
    cd config
    erl -noshell -name stop_feng@192.168.31.13 -setcookie test -config test -pa ../ebin -s test stop -extra 'feng@192.168.31.13'
}

# 调用函数
_CALL_FUNCTION(){
    local fname="fun"
    for arg in $@; do
        fname="${fname}_${arg}"
        shift
        if is_fun_exists ${fname}; then
            ${fname} $@
            exit 0
        fi
    done
    echo "无效的指令，请使用以下指令"
    fun_help
}

#-------------------------------------------------
# 脚本执行入口
#-------------------------------------------------
# 根据参数调用相应命令
_CALL_FUNCTION $@