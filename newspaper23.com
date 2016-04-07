<VirtualHost *:80>
        ServerAdmin admin@newspaper23.com
        ServerName newspaper23.com
        DocumentRoot /home/www/newspaper23.com
        <Directory />
                Options FollowSymLinks
                AllowOverride None
        </Directory>
        <Directory /home/www/newspaper23.com/>
                Options Indexes FollowSymLinks MultiViews
                AllowOverride FileInfo
                Order allow,deny
                allow from all
        </Directory>
	ScriptAlias /cgi-bin/ /home/www/newspaper23.com/cgi-bin/
	AddHandler cgi-script .cgi .pl .ml .fs .php
	<Directory /cgi-bin>
		Options +ExecCGI
	</Directory>
	ErrorLog /home/www/newspaper23.com/logs/error.log
	CustomLog /home/www/newspaper23.com/logs/access.log combined
</VirtualHost> 


<VirtualHost *:80>
        ServerAdmin admin@newspaper23.com
        ServerName www.newspaper23.com
        DocumentRoot /home/www/newspaper23.com
        <Directory />
                Options FollowSymLinks
                AllowOverride None
        </Directory>
        <Directory /home/www/newspaper23.com/>
                Options Indexes FollowSymLinks MultiViews
                AllowOverride FileInfo
                Order allow,deny
                allow from all
        </Directory>
	ScriptAlias /cgi-bin/ /home/www/newspaper23.com/cgi-bin/
	AddHandler cgi-script .cgi .pl .ml .fs .php
	<Directory /cgi-bin>
		Options +ExecCGI
	</Directory>
	ErrorLog /home/www/newspaper23.com/logs/error.log
	CustomLog /home/www/newspaper23.com/logs/access.log combined
</VirtualHost> 


<VirtualHost *:80>
        ServerAdmin admin@newspaper23.com
        ServerName newspaper23.org
        DocumentRoot /home/www/newspaper23.com
        <Directory />
                Options FollowSymLinks
                AllowOverride None
        </Directory>
        <Directory /home/www/newspaper23.com/>
                Options Indexes FollowSymLinks MultiViews
                AllowOverride FileInfo
                Order allow,deny
                allow from all
        </Directory>
	ScriptAlias /cgi-bin/ /home/www/newspaper23.com/cgi-bin/
	AddHandler cgi-script .cgi .pl .ml .fs .php
	<Directory /cgi-bin>
		Options +ExecCGI
	</Directory>
	ErrorLog /home/www/newspaper23.com/logs/error.log
	CustomLog /home/www/newspaper23.com/logs/access.log combined
</VirtualHost> 


<VirtualHost *:80>
        ServerAdmin admin@newspaper23.com
        ServerName www.newspaper23.org
        DocumentRoot /home/www/newspaper23.com
        <Directory />
                Options FollowSymLinks
                AllowOverride None
        </Directory>
        <Directory /home/www/newspaper23.com/>
                Options Indexes FollowSymLinks MultiViews
                AllowOverride FileInfo
                Order allow,deny
                allow from all
        </Directory>
	ScriptAlias /cgi-bin/ /home/www/newspaper23.com/cgi-bin/
	AddHandler cgi-script .cgi .pl .ml .fs .php
	<Directory /cgi-bin>
		Options +ExecCGI
	</Directory>
	ErrorLog /home/www/newspaper23.com/logs/error.log
	CustomLog /home/www/newspaper23.com/logs/access.log combined
</VirtualHost> 


<VirtualHost *:80>
        ServerAdmin admin@newspaper23.com
        ServerName newspaper23.biz
        DocumentRoot /home/www/newspaper23.com
        <Directory />
                Options FollowSymLinks
                AllowOverride None
        </Directory>
        <Directory /home/www/newspaper23.com/>
                Options Indexes FollowSymLinks MultiViews
                AllowOverride FileInfo
                Order allow,deny
                allow from all
        </Directory>
	ScriptAlias /cgi-bin/ /home/www/newspaper23.com/cgi-bin/
	AddHandler cgi-script .cgi .pl .ml .fs .php
	<Directory /cgi-bin>
		Options +ExecCGI
	</Directory>
	ErrorLog /home/www/newspaper23.com/logs/error.log
	CustomLog /home/www/newspaper23.com/logs/access.log combined
</VirtualHost> 


<VirtualHost *:80>
        ServerAdmin admin@newspaper23.com
        ServerName www.newspaper23.biz
        DocumentRoot /home/www/newspaper23.com
        <Directory />
                Options FollowSymLinks
                AllowOverride None
        </Directory>
        <Directory /home/www/newspaper23.com/>
                Options Indexes FollowSymLinks MultiViews
                AllowOverride FileInfo
                Order allow,deny
                allow from all
        </Directory>
	ScriptAlias /cgi-bin/ /home/www/newspaper23.com/cgi-bin/
	AddHandler cgi-script .cgi .pl .ml .fs .php
	<Directory /cgi-bin>
		Options +ExecCGI
	</Directory>
	ErrorLog /home/www/newspaper23.com/logs/error.log
	CustomLog /home/www/newspaper23.com/logs/access.log combined
</VirtualHost> 


<VirtualHost *:80>
        ServerAdmin admin@newspaper23.com
        ServerName newspaper23.us
        DocumentRoot /home/www/newspaper23.com
        <Directory />
                Options FollowSymLinks
                AllowOverride None
        </Directory>
        <Directory /home/www/newspaper23.com/>
                Options Indexes FollowSymLinks MultiViews
                AllowOverride FileInfo
                Order allow,deny
                allow from all
        </Directory>
	ScriptAlias /cgi-bin/ /home/www/newspaper23.com/cgi-bin/
	AddHandler cgi-script .cgi .pl .ml .fs .php
	<Directory /cgi-bin>
		Options +ExecCGI
	</Directory>
	ErrorLog /home/www/newspaper23.com/logs/error.log
	CustomLog /home/www/newspaper23.com/logs/access.log combined
</VirtualHost> 


<VirtualHost *:80>
        ServerAdmin admin@newspaper23.com
        ServerName www.newspaper23.us
        DocumentRoot /home/www/newspaper23.com
        <Directory />
                Options FollowSymLinks
                AllowOverride None
        </Directory>
        <Directory /home/www/newspaper23.com/>
                Options Indexes FollowSymLinks MultiViews
                AllowOverride FileInfo
                Order allow,deny
                allow from all
        </Directory>
	ScriptAlias /cgi-bin/ /home/www/newspaper23.com/cgi-bin/
	AddHandler cgi-script .cgi .pl .ml .fs .php
	<Directory /cgi-bin>
		Options +ExecCGI
	</Directory>
	ErrorLog /home/www/newspaper23.com/logs/error.log
	CustomLog /home/www/newspaper23.com/logs/access.log combined
</VirtualHost> 


<VirtualHost *:80>
        ServerAdmin admin@newspaper23.com
        ServerName newspaper23.mobi
        DocumentRoot /home/www/newspaper23.com
        <Directory />
                Options FollowSymLinks
                AllowOverride None
        </Directory>
        <Directory /home/www/newspaper23.com/>
                Options Indexes FollowSymLinks MultiViews
                AllowOverride FileInfo
                Order allow,deny
                allow from all
        </Directory>
	ScriptAlias /cgi-bin/ /home/www/newspaper23.com/cgi-bin/
	AddHandler cgi-script .cgi .pl .ml .fs .php
	<Directory /cgi-bin>
		Options +ExecCGI
	</Directory>
	ErrorLog /home/www/newspaper23.com/logs/error.log
	CustomLog /home/www/newspaper23.com/logs/access.log combined
</VirtualHost> 


<VirtualHost *:80>
        ServerAdmin admin@newspaper23.com
        ServerName www.newspaper23.mobi
        DocumentRoot /home/www/newspaper23.com
        <Directory />
                Options FollowSymLinks
                AllowOverride None
        </Directory>
        <Directory /home/www/newspaper23.com/>
                Options Indexes FollowSymLinks MultiViews
                AllowOverride FileInfo
                Order allow,deny
                allow from all
        </Directory>
	ScriptAlias /cgi-bin/ /home/www/newspaper23.com/cgi-bin/
	AddHandler cgi-script .cgi .pl .ml .fs .php
	<Directory /cgi-bin>
		Options +ExecCGI
	</Directory>
	ErrorLog /home/www/newspaper23.com/logs/error.log
	CustomLog /home/www/newspaper23.com/logs/access.log combined
</VirtualHost> 


<VirtualHost *:80>
        ServerAdmin admin@newspaper23.com
        ServerName newspaper23.net
        DocumentRoot /home/www/newspaper23.com
        <Directory />
                Options FollowSymLinks
                AllowOverride None
        </Directory>
        <Directory /home/www/newspaper23.com/>
                Options Indexes FollowSymLinks MultiViews
                AllowOverride FileInfo
                Order allow,deny
                allow from all
        </Directory>
	ScriptAlias /cgi-bin/ /home/www/newspaper23.com/cgi-bin/
	AddHandler cgi-script .cgi .pl .ml .fs .php
	<Directory /cgi-bin>
		Options +ExecCGI
	</Directory>
	ErrorLog /home/www/newspaper23.com/logs/error.log
	CustomLog /home/www/newspaper23.com/logs/access.log combined
</VirtualHost> 


<VirtualHost *:80>
        ServerAdmin admin@newspaper23.com
        ServerName www.newspaper23.net
        DocumentRoot /home/www/newspaper23.com
        <Directory />
                Options FollowSymLinks
                AllowOverride None
        </Directory>
        <Directory /home/www/newspaper23.com/>
                Options Indexes FollowSymLinks MultiViews
                AllowOverride FileInfo
                Order allow,deny
                allow from all
        </Directory>
	ScriptAlias /cgi-bin/ /home/www/newspaper23.com/cgi-bin/
	AddHandler cgi-script .cgi .pl .ml .fs .php
	<Directory /cgi-bin>
		Options +ExecCGI
	</Directory>
	ErrorLog /home/www/newspaper23.com/logs/error.log
	CustomLog /home/www/newspaper23.com/logs/access.log combined
</VirtualHost> 


<VirtualHost *:80>
        ServerAdmin admin@newspaper23.com
        ServerName newspaper23.info
        DocumentRoot /home/www/newspaper23.com
        <Directory />
                Options FollowSymLinks
                AllowOverride None
        </Directory>
        <Directory /home/www/newspaper23.com/>
                Options Indexes FollowSymLinks MultiViews
                AllowOverride FileInfo
                Order allow,deny
                allow from all
        </Directory>
	ScriptAlias /cgi-bin/ /home/www/newspaper23.com/cgi-bin/
	AddHandler cgi-script .cgi .pl .ml .fs .php
	<Directory /cgi-bin>
		Options +ExecCGI
	</Directory>
	ErrorLog /home/www/newspaper23.com/logs/error.log
	CustomLog /home/www/newspaper23.com/logs/access.log combined
</VirtualHost> 


<VirtualHost *:80>
        ServerAdmin admin@newspaper23.com
        ServerName www.newspaper23.info
        DocumentRoot /home/www/newspaper23.com
        <Directory />
                Options FollowSymLinks
                AllowOverride None
        </Directory>
        <Directory /home/www/newspaper23.com/>
                Options Indexes FollowSymLinks MultiViews
                AllowOverride FileInfo
                Order allow,deny
                allow from all
        </Directory>
	ScriptAlias /cgi-bin/ /home/www/newspaper23.com/cgi-bin/
	AddHandler cgi-script .cgi .pl .ml .fs .php
	<Directory /cgi-bin>
		Options +ExecCGI
	</Directory>
	ErrorLog /home/www/newspaper23.com/logs/error.log
	CustomLog /home/www/newspaper23.com/logs/access.log combined
</VirtualHost> 


