/**

Livewire - Core code for running the Livewire algorithm
Copyright (C) 2011  Jeffrey Bush  jeff@coderforlife.com

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

**/

#ifndef THREADED_H
#define THREADED_H

#include "general.h"

#include <QThread>
#include <QMutex>

#ifdef _DEBUG
#include <QElapsedTimer>
#endif

namespace Livewire
{
	class Threaded : public QThread
	{
		Q_OBJECT

	private:
		QString _name;
		QMutex _lock;
#ifdef _DEBUG
		QElapsedTimer _timer;
#endif
		bool _isExecuting;
		int _progress, _partialProgress, _inc, _totalProgress;

	protected:
		Threaded(char *name);
		~Threaded();

	protected:
		void SetProgress(int value);
		void SetTotalProgress(int value);
		void IncProgress();
	public:
		int GetProgress();
		int GetTotalProgress();

	protected:
		void Checkpoint(char *info);
	public:
		bool IsExecuting();

	protected:
		void Start();
		void run();
		virtual void Run() = 0;
		void Stop(bool wait);
	public:
		virtual void Stop();

	signals:
		void ProgressChanged(int value);
	};
}

#endif
