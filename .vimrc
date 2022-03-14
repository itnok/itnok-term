" __     _____ __  __
" \ \   / /_ _|  \/  |
"  \ \ / / | || |\/| |
"   \ V /  | || |  | |
"  _ \_/__|___|_|  |_|  _ _ __
" | '_ ` _ \ / _ \ | | | | '_ \
" | | | | | |  __/ | |_| | |_) |
" |_|_|_| |_|\___| _\__,_| .__/
" / __| __ ___| |_| |_ _ |_|
" \__ \/ _/ _ \  _|  _| || |
" |___/\__\___/\__|\__|\_, |
"    ___-___  o==o======|__/
" =========== ||//
"         \ \ |//__
"         #_______/


"  _
" |_|\ |\  /
" |_| \| \/
"
" #pragma VIM:ENV
" ---
" Check for customization options coming from env variables
" ---
" Set path to vim data/config directory
if !empty($VIM_DATA_DIR_NAME)
  let g:vim_data_path = fnameescape('~/' . tolower($VIM_DATA_DIR_NAME))
else
  let g:vim_data_path = '~/.vim'
endif
" ==> Set path to vim plugins
if !empty($VIM_PLUGIN_DIR_NAME) && tolower($VIM_PLUGIN_DIR_NAME) != 'plugin'
  let g:vim_plugins_path = fnameescape(g:vim_data_path . '/' . tolower($VIM_PLUGIN_DIR_NAME))
else
  let g:vim_plugins_path = g:vim_data_path . '/plugged'
endif
" ==> Set path to vim configuration file
if !empty($VIM_CONF_FILE_NAME)
  let g:vim_config_path = fnameescape(g:vim_data_path . '/' . tolower($VIM_CONF_FILE_NAME))
else
  let g:vim_config_path = g:vim_data_path . '/vimrc.conf'
endif
" ==> Set path to vim pre-configuration file
if !empty($VIM_CONF_BEFORE_FILE_NAME)
  let g:vim_config_before_path = fnameescape(g:vim_data_path . '/' . tolower($VIM_CONF_BEFORE_FILE_NAME))
else
  let g:vim_config_before_path = g:vim_data_path . '/vimrc.befire.conf'
endif
" ==> Set path to vim post-configuration file
if !empty($VIM_CONF_AFTER_FILE_NAME)
  let g:vim_config_after_path = fnameescape(g:vim_data_path . '/' . tolower($VIM_CONF_AFTER_FILE_NAME))
else
  let g:vim_config_after_path = g:vim_data_path . '/vimrc.after.conf'
endif
" ==> Set expected name for custom local (per-project) configuration files
if !empty($VIM_CONF_CUSTOM_FILE_NAME)
  let g:vim_config_custom_name = fnameescape($VIM_CONF_CUSTOM_FILE_NAME)
else
  let g:vim_config_custom_name = '.vimrc.custom'
endif
" ==> Set the path to the plugin configuration file
let g:vim_config_plugin_path = g:vim_data_path . '/plug.conf'
" ==> Set the path to the current work directory before any possible change
let g:vim_current_work_path = getcwd()


"  _
" | \ __|_ _  __|_
" |_/(/_|_(/_(_ |_
" 
" #pragma VIM:Detect
" ---
" Detect relevant aspects of the environment VIM is in
" ---
" ==> Detect Python3 version
if has("python3")
  python3 import vim;
        \ from sys import version_info as v;
        \ vim.command('let g:python3_version = %d' % (v[0] * 100 + v[1]))
else
  let g:python3_version = 0
endif


"  _
" |_._  _ _  _|o._  _
" |_| |(_(_)(_||| |(_|
"                   _|
" #pragma VIM:Encoding
" ---
" Set default text encoding to make sure it is always UTF-8
" ---
" ==> Set the encoding for display
set encoding=utf-8
" ==> Set encoding for writing to file
set fileencoding=utf-8


"  _       _
" /  _ .__|_o _
" \_(_)| || |(_|
"             _|
" #pragma VIM:Config
" ---
" Main configuration handling (through loading external files)
" ---
" ==> Use the modular configuration approach only on vim8 or newer
"     and only when the main configuration is available
if v:version >= 800 && filereadable(expand(g:vim_config_path))
  " ==> Load the custom configuration settings to apply BEFORE the main ones
  if filereadable(expand(g:vim_config_before_path))
    exec 'source ' . expand(g:vim_config_before_path)
  endif


  "  _
  " |_)|    _ o._
  " |  ||_|(_||| |
  "         _|
  " #pragma VIM:Config:Plugin
  " ---
  " Autoinstall and configure the Plug plugin manager
  " (Plugins will be loaded using the settings present in the 'plug.conf'
  " file stored in the configured `g:vim_data_path` directory)
  " ---
    if python3_version >= 306
      " ==> Try to auto-install vim-plug if missing
      let autoload_vimplug_path = expand(g:vim_data_path . '/autoload/plug.vim')
      if empty(glob(autoload_vimplug_path))
        silent exec '!curl -sfL --create-dirs -o ' . autoload_vimplug_path .
          \ ' https://raw.github.com/junegunn/vim-plug/master/plug.vim'
        exec 'source ' . autoload_vimplug_path
      endif
      unlet autoload_vimplug_path
      " ==> Load the Plug plugins array used to go over all plugins to use
      if filereadable(expand(g:vim_config_plugin_path))
        exec 'source ' . expand(g:vim_config_plugin_path)
      endif
      " ==> Configure the directory used for storing plugins
      "     (should not be 'plugin'!)
      call plug#begin(g:vim_plugins_path)

      " ==> Dynamically load plugin list and their configuration from file
      "     and let Plug handle them...
      if empty(plug_plugin_list)
        let plug_plugin_list = []
      endif
      for plugin in plug_plugin_list
        if has_key(plugin, 'name')
          let idx = 0
          let plug_cmd = ''
          if !has_key(plugin, 'conf')
            let plugin_conf = []
          else
            let plugin_conf = plugin['conf']
          endif
          if !has_key(plugin, 'if')
            let plugin_if = 1
          else
            let plugin_if = plugin['if']
          endif
          if plugin_if
            for value in plugin['name']
              let plug_cmd = plug_cmd . "Plug '" 
                                    \ . value
                                    \ . "', "
                                    \ . trim(execute('echo get(plugin_conf, idx, {})'))
                                    \ . ' | '
              let idx += 1
            endfor
            let plug_cmd = substitute(plug_cmd, ' | $', '', 'g')
            exec plug_cmd
          endif
          unlet idx
          unlet plug_cmd
          unlet plugin_conf
        endif
      endfor

      " ==> END of plugin system initialization
      call plug#end()
      " ==> [\p] Set shortcut to quickly force Plug upgrade to its latest
      "     version and also force all plugins update
      nnoremap <leader>pu :PlugUpgrade <bar> :PlugUpdate<CR>
  endif

  " ==> Load the MAIN CONFIGURATION
  exec 'source ' . expand(g:vim_config_path)

  " ==> Load the custom configuration settings to apply AFTER the main ones
  if filereadable(expand(g:vim_config_after_path))
    exec 'source ' . expand(g:vim_config_after_path)
  endif

  " ==> Load CUSTOM LOCAL configuration present in the path from where
  "     vim was run
  if filereadable(expand(g:vim_current_work_path . '/' . g:vim_config_custom_name))
    exec 'source ' . expand(g:vim_current_work_path . '/' . g:vim_config_custom_name)
  endif
endif
