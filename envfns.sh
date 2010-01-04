######################################################################
#
# $Id: envfns.sh,v 1.1 2008/04/30 14:16:28 mort Exp $

#
# delete $2 from env. var. $1
#

denv () {
        if [ $# != 2 ]; then
                echo "Usage: denv <env_var> <value>"
                return 1
        fi

        ##        denv_tmp=$(eval echo \"\$\{$1\}\") # for sh compatability

        denv_tmp=${!1} ## bash v2+
        case "$denv_tmp" in
          *:${2}:* ) 
                   denv_tmp=${denv_tmp%:${2}:*}:${denv_tmp#*:${2}:}
                   ;;

          *:${2}   )
                   denv_tmp=${denv_tmp%:${2}}
                   ;;

          ${2}:*   )
                   denv_tmp=${denv_tmp#${2}:}
                   ;;

          ${2}     )
                   denv_tmp=""
                   ;;
        esac

        eval ${1}=$denv_tmp 
        export ${1}
}

#
# add $2 to env. var $1 at start
#

aenv () {
        if [ $# != 2 ]; then
                echo "Usage: aenv <env_var> <value>"
                return 1
        fi

        denv "$1" "$2"
        eval ${1}="$2":$(echo $(eval echo "$\{$1\}"))
        export ${1}
}

#
# add $2 to env. var $1 at end
#

enva () {
        if [ $# != 2 ]; then
                echo "Usage: aenv <env_var> <value>"
                return 1
        fi

        denv $1 $2
        eval ${1}=$(echo $(eval echo $\{$1\})):$2
        export ${1}
}

#
# remove repeats in env. var. $1
#

cenv () {
        if [ $# != 1 ]; then
                echo "Usage: cenv <env_var>"
                return 1
        fi
        
        cenv_tmp=$(eval echo $\{$1\}) ;

        while [ "$cenv_tmp" != "" ] ; do 
                aenv $1 ${cenv_tmp##*:} > /dev/null 

                # really want to "denv cenv_tmp {wotsit} but 4 sed procs?!
                if [ "$cenv_tmp" = "${cenv_tmp%:*}" ]; then 
                        cenv_tmp="" ; 
                else 
                        cenv_tmp=${cenv_tmp%:*} ; 
                fi ; 
        done
}
