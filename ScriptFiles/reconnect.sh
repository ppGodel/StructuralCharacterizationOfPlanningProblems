while [ 1 ]; do
    ping -c 1 192.168.47.1 & wait $!    
    if [ $? != 0 ]; then
        echo $(date)" attempting restart..." >> $log
        sh ~/td.sh
	sh ~/tu.sh
        sleep 1
    else sleep 60
    fi
done
